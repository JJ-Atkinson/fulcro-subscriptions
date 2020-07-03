(ns dev.freeformsoftware.fulcro-subscriptions.subscription-register
  "Location of the default subscription register. Public interface is .
   This can be swapped out for an alternate definition if required."

  (:require [com.fulcrologic.guardrails.core :refer [>defn => >def]]
            [clojure.spec.alpha :as s]))




(defonce subscription-fns (atom #{}))





(>defn register-sub-fn!
  "Accepts a subscription, stores it. Default behavior is to use the default `invoke-sub!` 
   to get the initial value. If you have swapped that out, partial this function so you can use
   the other implementation."
  ([sub]
   [any? => any?]
   (register-sub-fn! nil sub))
  ([invoke-fn sub]
   [ifn? any? => any?]
   (swap! subscription-fns conj sub)
   (when invoke-fn (invoke-fn sub))))



(>defn deregister-sub-fn!
  "Accepts a subscription, and removes it from the db."
  ([sub]
   [any? => any?]
   (swap! subscription-fns disj sub)))


(>defn all-subs 
  "This is the default all-subs fn. This allows you to replace this ns with some other backend."
  []
  [=> seq?]
  (seq @subscription-fns))

