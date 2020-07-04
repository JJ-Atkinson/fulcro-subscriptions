(ns dev.freeformsoftware.fulcro-subscriptions.subscription
  "This is where subscription functions are created. There are actually multiple layers of abstraction
   here. At the most basic level, this namespace could be skipped entirely. All that the hooks
   and register require is some function that given the app-db of fulcro returns the following map:
   {:unchanged? boolean
    :res        the result of the subscription, passed to the subscriber}
    
   However, usually it is nice to step back a bit while constructing these functions. Intermediate signal
   layers are quite nice for performance, and the ability to specify the graph in a distributed fashion
   is quite nice. It is quite easy to do something like this though:
   
   
   ```clojure
   (defn count-sales [sales-table] (count sales-table))
   (defn extract-sales-table [app-db] (:sale/id app-db))
   (defn count-sales* [app-db] (-> app-db extract-sales-table count-sales))
   
   (defn wrap-sub [f]
     (fn [app-db]
       {:res        (f app-db)
        :unchanged? false}))
        
   (def count-sales-sub (wrap-sub count-sales*))
        
        
   (defsc SalesList ...
     (use-sub count-sales-sub)
     ...)
   ```
     
     "
  (:require [com.fulcrologic.guardrails.core :refer [>defn => >def ?]]
            [clojure.spec.alpha :as s]
            [dev.freeformsoftware.fulcro-subscriptions.dependencies.deps-sorted :as dep.sorted]
            [com.stuartsierra.dependency :as dep]
            [clojure.set :as set]
            ))


(>def ::subscription-reference #(or (keyword? %) (symbol? %)))

(>def ::f ifn?)
(>def ::arguments (s/coll-of ::subscription-reference))
(>def ::returns ::subscription-reference)
(>def ::invocation-fn ifn?)

(>def ::subscription-description (s/keys :req [::f ::arguments ::returns ::invocation-fn]))



(defn linear-apply-f
  "Apply f to a linear based function. e.g.
    {::f             (fn [a b c] ...)
     ::arguments     [:a :b :c]
     ::invocation-fn linear-apply-f)"
  [f arg-list arg-map]
  (apply f (map #(get arg-map %) arg-list)))

(defn map-apply-f
  "Apply f to a map based function. e.g.
  {::f             (fn [{:keys [a b c]}] ...)
   ::arguments     [:a :b :c]
   ::invocation-fn map-apply-f)"
  [f arg-list arg-map]
  (f arg-map))


(>defn invoke-subscription-definition
  "Not the same as `apply-description-definition`. This will take an argument map and apply it as the arguments
   to the subscription definition. Will directly return the value of the function."
  [sub-def args]
  [::subscription-description (? map?) => any?]
  ((::invocation-fn sub-def) args))

(>defn apply-description-definition
  "Not the same as `invoke-subscription-definition`. This will take an argument map and apply it as the arguments
   to the subscription definition. Will return the original args with the result of the invocation added to the map."
  ;; not currently used in any implementation, but could be useful at some point.
  [sub-def args]
  [::subscription-description (? map?) => map?]
  (assoc args
    (::returns sub-def) (invoke-subscription-definition sub-def args)))


(>defn describe-function
  "This is the most basic layer of the default engine. Given a description of the arguments, return, and
   function, create a description that can be used to dynamically invoke the function given an argument
   map. Also used in dependency resolution.
   
   Since functions can sometimes take a map of arguments, and sometimes take the arguments linearly, there
   is an option to switch the function used to apply f to an argument map. Built in options are
   [[linear-apply-f]] and [[map-apply-f]]."
  ([f args ret]
   [ifn? (s/coll-of ::subscription-reference) ::subscription-reference => ::subscription-description]
   (describe-function f args ret (partial linear-apply-f f args)))
  ([f args ret invocation-fn]
   [ifn? (s/coll-of ::subscription-reference) ::subscription-reference ifn? => ::subscription-description]
   {::f             f
    ::arguments     args
    ::returns       ret
    ::invocation-fn invocation-fn}))




;; handles 2 things: the dependency graph and the function register
(defonce subscription-description-registry
  (atom nil))

(defn reset-subscription-description-register!
  "Unlikely to be used outside of tests."
  []
  (reset! subscription-description-registry {::dependency-graph (dep/graph)
                                             ::descriptions     {}}))
(reset-subscription-description-register!)



(>defn chain-descriptions
  "This is a stateful operation by default. This will attempt to chain the functions in invocation order
   based on the current contents of the subscription-description register."
  ([goal-reference]
   [map? => (s/coll-of ::subscription-description)]
   (chain-descriptions goal-reference #{}))
  ([goal-reference pre-resolved]
   [::subscription-reference set? => (s/coll-of ::subscription-description)]
   (chain-descriptions @subscription-description-registry goal-reference pre-resolved))
  ([subscription-description-register goal-reference pre-resolved]
   [map? ::subscription-reference set? => (s/coll-of ::subscription-description)]
   (let [deps (dep.sorted/full-dependencies-set
                (::dependency-graph subscription-description-register)
                #{goal-reference}
                (set pre-resolved))
         desc (::descriptions subscription-description-register)
         fns (map #(get desc %) deps)]
     fns)))


(>defn register-subscription-description
  "USAGE NOTE: there is an implicit function pre-registered called ::app-db. It is a no dependency function
   that jut uses the app db provided by the subscription runner plugin added to fulcro.
   
   This returns a delay"
  ([subscription-description]
   [::subscription-description => any?]
   (let [{::keys [arguments returns]} subscription-description
         {::keys [dependency-graph descriptions]} @subscription-description-registry
         dep-graph (reduce (fn [g arg] (dep/depend g returns arg))
                     dependency-graph arguments)]
     (reset! subscription-description-registry
       {::dependency-graph dep-graph
        ::descriptions     (assoc descriptions
                             returns subscription-description)}))))




(>defn simple-invocation-strategy
  "This takes a description chain and returns 
   (fn [starting-map] final-result). It won't return the intermediate map, only the result of the last function
   invocation.
   
   
   ```clojure
   (def f1 (describe-function str [:a :b] :c))
   (def f2 (describe-function str [:a :b :c] :d))
   
   (defn f-all (simple-invocation-strategy [f1 f2]))
   
   (f-all {:a \"a\" :b \"b\"})
   ; => {:res \"ababc\" :unchanged? false}
   
   This will not do anything more than naively apply the functions. If you want diff/no-diff, look at 
   [[short-circuit-invocation-strategy]] "
  [chain]
  [(s/coll-of ::subscription-description) => [map? => any?]]
  (fn simple-invocation-strategy* [context-map]
    (loop [[current-sub & rest] chain
           run-result context-map]
      (let [new-res (invoke-subscription-definition current-sub run-result)
            run-result (assoc run-result
                         (::returns current-sub)
                         new-res)]
        (if-not (empty? rest)
          (recur rest run-result)
          {:res new-res :unchanged? false})))))

(defn lspy [& x] (println x) (last x))



(>defn short-circuit-invocation-strategy
  "This takes a description chain and returns 
   (fn [starting-map] final-result). It won't return the intermediate map, only the result of the last function
   invocation.
   
   
   ```clojure
   (def f1 (describe-function pos? [:a] :pos?))
   (def f2 (describe-function str [:pos?] :res))
   
   (defn f-all (short-circuit-invocation-strategy [f1 f2]))
   
   (f-all {:a 1})
   ; => {:res \"true\" :unchanged? false}
   (f-all {:a 1})   
   ; => {:res \"true\" :unchanged? true}
   (f-all {:a -1}
   ; => {:res \"false\" :unchanged? false}"
  [chain]
  ; ifn? used because gspecs generative testing was run killing the internal state
  [(s/coll-of ::subscription-description) => ifn?]
  (let [changed?-map (volatile! (transient {}))
        last-result (volatile! (transient {}))]
    (fn short-circuit-invocation-strategy* [context-map]
      (loop [[curr-fn & remaining-fns] (seq chain)
             accumulated-result context-map]
        (let [val-loc (::returns curr-fn)
              changed-args? (some #(get @changed?-map % true) (::arguments curr-fn))
              val (if changed-args?
                    (invoke-subscription-definition curr-fn accumulated-result)
                    (get @last-result val-loc))
              accumulated-result (assoc accumulated-result val-loc val)
              val-changed? (and changed-args? (not= val (get @last-result val-loc)))]
          (vreset! changed?-map (assoc! @changed?-map val-loc val-changed?))
          (vreset! last-result (assoc! @last-result val-loc val))
          (if (seq remaining-fns)
            (recur remaining-fns accumulated-result)
            {:res val :unchanged? (not val-changed?)}))))))


(>defn instance-subscription
  "Take some subscription name, resolve it to a list of dependencies, wrap it in
   some invocation strategy. Optionally accepts a `:pre-resolved-set` of keys that 
   the dependency resolver can skip. Also optionally accepts some `:initial-args-map`.
   If the `:initial-args-map` is not nil and the `:pre-resolved-set` of keys
   set is nil, uses the keyset of `:initial-args-map`. Use `{:pre-resolved-set false}`
   to cancel this. 
   
   By default this will use the `short-circuit-invocation-strategy`. The utility of
   of using this strategy can be nullified when using memoization on some resolvers.
   Ultimately either changing which nodes have what kind of memoization and implementing
   custom invocation strategies should make performance tuning easier."
  ([name]
   [::subscription-reference => ifn?]
   (instance-subscription name {}))
  ([name {:as opts :keys [pre-resolved-set initial-args-map invocation-strategy]
          :or {invocation-strategy short-circuit-invocation-strategy
               initial-args-map    {}}}]
   [::subscription-reference map? => ifn?]
   (let [pre-resolved-set (if (nil? pre-resolved-set)
                            (set (keys initial-args-map))
                            pre-resolved-set)
         resolved-fns (chain-descriptions name pre-resolved-set)
         runner (invocation-strategy resolved-fns)]
     ;; just prevent call chains getting deeper without cause
     (if (seq initial-args-map)
       (fn wrap-merge-initial-args* [args]
         (runner (merge args initial-args-map)))
       runner))))
