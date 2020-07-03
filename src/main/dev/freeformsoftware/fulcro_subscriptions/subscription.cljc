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
            [com.stuartsierra.dependency :as dep]))



(>def ::f ifn?)
(>def ::arguments (s/coll-of keyword?))
(>def ::returns keyword?)
(>def ::invocation-fn ifn?)

(>def ::subscription-description (s/keys :req [::f ::arguments ::returns]))



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
   [ifn? (s/coll-of keyword?) keyword? => ::subscription-description]
   (describe-function f args ret (partial linear-apply-f f args)))
  ([f args ret invocation-fn]
   [ifn? (s/coll-of keyword?) keyword? ifn? => ::subscription-description]
   {::f             f
    ::arguments     args
    ::returns       ret
    ::invocation-fn invocation-fn}))


;; handles 2 things: the dependency graph and the functions
(defonce subscription-description-register 
  (atom {::dependency-graph nil
         ::descriptions {}}))


(defn register-subscription-definition [])


(comment (->
           (dep/graph)
           (dep/depend :b :a)
           (dep/depend :a :c)
           (dep/transitive-dependencies :b)))