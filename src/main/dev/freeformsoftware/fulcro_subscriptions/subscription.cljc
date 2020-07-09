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
            #?(:cljs [dev.freeformsoftware.fulcro-subscriptions.hooks :as hooks])
            [clojure.set :as set])
  #?(:cljs (:require-macros [dev.freeformsoftware.fulcro-subscriptions.subscription])))

(declare application-strategies)
(declare application-functions)

(>def ::subscription-reference
  "Subscriptions can be either named by keywords or symbols depending on the section of the api 
   you decide to work with. "
  #(or (keyword? %) (symbol? %)))

(>def ::f
  "The root function passed to the whole system. Everything revolves around the function given here.
   Can have any api structure you desire, but it must be wrapped in an ::invocation-fn such that
   (fn [all-args] result) is returned. Should not return the map with the result assoced on. "
  ifn?)
(>def ::providers
  "Collection of references to which keys the sub depends on."
  (s/coll-of ::subscription-reference))
(>def ::outputs
  "What does the subscription provided?"
  ::subscription-reference)
(>def ::invocation-fn
  "See ::f"
  (s/or :function ifn? :keyword #(contains? application-functions %)))

(>def ::name
  "The defprovider macro is more restrictive in naming. (idea - maybe expand the api definition
   so that defprovider actually creates a keyword and a symbol version)"
  symbol?)
(>def ::invocation-strategy
  "Invocation strategies are ways to convert a list of ::subscription-description into a form consumable
   by a render api. See the implementation of the default of both for more details.
   (the reason to support keywords is as a shorthand for the default functions via keyword)"
  (s/or :function ifn? :keyword #(contains? application-functions %)))
(>def ::params set?)

(>def ::subscription-description (s/keys :req [::f ::providers ::outputs ::invocation-fn]))



;; Describe nice shortcuts to go from random function to a function that works on a map


(defn linear-apply-f
  "Apply f to a linear based function. e.g.
    {::f             (fn [a b c] ...)
     ::inputs        [:a :b :c]
     ::invocation-fn linear-apply-f)"
  [f arg-list arg-map]
  (apply f (map #(get arg-map %) arg-list)))

(defn map-apply-f
  "Apply f to a map based function. e.g.
  {::f             (fn [{:keys [a b c]}] ...)
   ::inputs        [:a :b :c]
   ::invocation-fn map-apply-f)"
  [f arg-list arg-map]
  (f arg-map))


(def ^:private application-functions {:linear-apply linear-apply-f
                                      :map-apply    map-apply-f})


;; Nice way to take some subscription definition and apply it to a map

(>defn invoke-subscription-definition
  "Not the same as `apply-description-definition`. This will take an argument map and apply it as the inputs
   to the subscription definition. Will directly return the value of the function."
  [sub-def args]
  [::subscription-description (? map?) => any?]
  ((::invocation-fn sub-def) args))

(>defn apply-description-definition
  "Not the same as `invoke-subscription-definition`. This will take an argument map and apply it as the inputs
   to the subscription definition. Will return the original args with the result of the invocation added to the map."
  ;; not currently used in any implementation, but could be useful at some point.
  [sub-def args]
  [::subscription-description (? map?) => map?]
  (assoc args
    (::outputs sub-def) (invoke-subscription-definition sub-def args)))


;; Core of the api. Describe how a function behaves in the tree. This is what is reified
;; when registering a subscription node

(>defn describe-function
  "This is the most basic layer of the default engine. Given a description of the inputs, return, and
   function, create a description that can be used to dynamically invoke the function given an argument
   map. Also used in dependency resolution.
   
   Since functions can sometimes take a map of inputs, and sometimes take the inputs linearly, there
   is an option to switch the function used to apply f to an argument map. Built in options are
   [[linear-apply-f]] and [[map-apply-f]]."
  ([f args ret]
   [ifn? (s/coll-of ::subscription-reference) ::subscription-reference => ::subscription-description]
   (describe-function f args ret linear-apply-f))
  ([f args ret invocation-fn]
   [ifn? (s/coll-of ::subscription-reference) ::subscription-reference any? => ::subscription-description]
   {::f             f
    ::providers     args
    ::outputs       ret
    ::invocation-fn (partial (get application-functions invocation-fn invocation-fn)
                      f args)}))




;; handles 2 things: the dependency graph and the function register
(defonce subscription-description-registry
  (atom nil))

(defn reset-subscription-description-register!
  "Unlikely to be used outside of tests."
  []
  (reset! subscription-description-registry {::dependency-graph (dep/graph)
                                             ::descriptions     {}}))
(reset-subscription-description-register!)


(>defn register-subscription-description
  "USAGE NOTE: there is an implicit function pre-registered called ::app-db. It is a no dependency function
   that jut uses the app db provided by the subscription runner plugin added to fulcro.
   "
  ([subscription-description]
   [::subscription-description => any?]
   (let [{::keys [providers outputs]} subscription-description
         {::keys [dependency-graph descriptions]} @subscription-description-registry
         dep-graph (reduce (fn [g arg] (dep/depend g outputs arg))
                     dependency-graph providers)]
     (reset! subscription-description-registry
       {::dependency-graph dep-graph
        ::descriptions     (assoc descriptions
                             outputs subscription-description)}))))


;; Create a list of descriptions in proper execution order with some options


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
     (remove nil? fns))))


;; Create a way to run a list of descriptions against a map. These implementations support the 
;; api handled in simple-render-fn of {:unchanged? boolean} to speed execution

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
                         (::outputs current-sub)
                         new-res)]
        (if-not (empty? rest)
          (recur rest run-result)
          {:res new-res :unchanged? false})))))



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
        (let [val-loc (::outputs curr-fn)
              changed-args? (some #(get @changed?-map % true) (::providers curr-fn))
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


(def ^:private application-strategies {:simple-invocation-strategy        simple-invocation-strategy
                                       :short-circuit-invocation-strategy short-circuit-invocation-strategy})


;; nice api that collects several concerns together (with some sane defaults)
;;  - Creating a chain of functions based off of registered subscriptions
;;  - How to manage initial arguments
;;  - Avoiding pre-resolved keys


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


;; Here we get to the meat. This is the most used part of the interface, allowing you
;; to easily add subscription functions to the general api and then use them inside
;; components. 


(>def ::subscription-map (s/keys :opt [::providers
                                       ::invocation-strategy
                                       ::invocation-fn
                                       ::params]))


(>def ::dependency-chain
  "Should be a deref, but not sure how to spec that."
  any?)
(>def ::subscription-map-ref (s/merge ::subscription-map
                               (s/keys :req [::outputs
                                             ::dependency-chain
                                             ::invocation-strategy])))

(def ^:dynamic *subscription-defaults*
  {::invocation-fn       linear-apply-f
   ::params              #{::app-db}
   ::invocation-strategy short-circuit-invocation-strategy})

(>defn defprovider*
  "Backend implementation of the macro defprovider. This has almost the exact
   same shape api, and can be used interchangeably. The defusbscription macro is
   simply sugar that will make it easier to identify. Because of how this function is used though,
   it has a slightly less restrictive api than defprovider in that the name can be a keyword. Names
   must be symbols for the macro. PSA: this is only ever executed at runtime. No information
   is currently available at compile time about which subscriptions are created.
   
   An issue arises here though - how do I support ::pre-resolved-keys (used for passing in arguments
   to the subscription, like an ident or something) which have the ability to elide
   parts of the subscription tree? The ultimate decision is that the most likely use case of 
   ::pre-resolved-keys is to prevent looking for subscriptions that don't exist in the tree,
   and soley are used outside. This enables a strategy of compiling the tree and then pruning
   off all the elements that are in pre-provided-keys.
   
   Returns a map of the following structure (this map is used in the [[subscribe]] function
   in this namespace to resolve all the required functions.). This structure is also sent
   to the register atom. 
   
   
   {::inputs      Vector of dependencies
    ::outputs "
  [f name opts]
  [ifn? ::subscription-reference ::subscription-map => ::subscription-map-ref]
  (let [opts (assoc opts ::outputs name)
        opts (merge *subscription-defaults* opts)
        opts (merge
               opts
               (describe-function f (::providers opts) name (::invocation-fn opts)))]
    (register-subscription-description opts)
    (assoc opts
      ::dependency-chain (delay (let [_ (println "trying resolve" name)
                                      descriptions (chain-descriptions name)
                                      _ (println descriptions)
                                      pre-resolved (apply set/union
                                                     (map ::params descriptions))
                                      pruned (remove #(or (contains? pre-resolved (::outputs %))
                                                        (nil? %))
                                               descriptions)]
                                  pruned)))))



(s/def ::defprovider-arg-map
  (s/cat
    :sym ::name
    :doc (s/? string?)
    :argument-vector vector?
    :opt-map ::subscription-map
    :body any?))

#?(:clj
   (defmacro defprovider
     "See defprovider*"
     [& args]
     (let [clean-symbol (fn [s?]
                          (cond (keyword? s?) s?
                                (namespace s?) s?
                                :default (symbol (name (ns-name *ns*)) (name s?))))
           quote-symbol (fn [s?] (if (symbol? s?) `(quote ~s?) s?))

           {:keys [sym doc argument-vector opt-map body]
            :or   {doc ""} :as args} (s/conform ::defprovider-arg-map args)
           clean-inputs (update opt-map ::providers (fn [x] (mapv (comp quote-symbol clean-symbol) x)))]
       (let [fqsym (clean-symbol sym)]
         `(def ~sym ~doc
            (dev.freeformsoftware.fulcro-subscriptions.subscription/defprovider*
              (fn ~sym ~argument-vector ~body) '~fqsym ~clean-inputs))))))

#?(:cljs
   (>defn subscribe!
     "Take some defprovider (and optionally some context arguments) to generate a 
      subscription using hooks. Make a new function to use something other than the default
      hooks implementation. Supports the same args as `use-sub` inside the defprovider"
     ([appish reference]
      [any? ::subscription-map-ref => any?]
      (subscribe! appish reference nil))
     ([appish reference initial-args]
      [any? ::subscription-map-ref (? map?) => any?]
      (let [{::keys [invocation-strategy dependency-chain]} reference]
        (hooks/use-sub
          (fn []
            (let [is (invocation-strategy @dependency-chain)]
              (if initial-args
                (fn intermediate-merge* [args] (is (merge initial-args args)))
                is)))
          (assoc reference :lazy? true :appish appish))))))