(ns dev.freeformsoftware.fulcro-subscriptions.hooks
  (:require [com.fulcrologic.fulcro.react.hooks :as hooks]
            [dev.freeformsoftware.fulcro-subscriptions.subscription-register :as sub-reg]
            [dev.freeformsoftware.fulcro-subscriptions.simple-render-fn :as sfr]
            [com.fulcrologic.fulcro.components :as comp]
            [com.fulcrologic.fulcro.application :as application]))

(defn use-sub
  "Sub-fns are potentially expensive things to create, so you can use the lazy? flag to indicate
   that you are passing a no arg function that creates the sub. 
   
   This encapsulates the entire lifecycle of a hook. The set! function is never returned to you,
   only the current value. This does not do anything to determine the initial value - the register
   has that job.  
   
   This implementation will just push/drop from the global register. If you want to use a 
   custom location, you will need to alter both this function and the render function.
   It passes this into the register:
   {:calc-fn   subscription-fn
    :set-fn!    function to set the value of the hook}
    
   To get the initial value created, you should pass in some app-ish state to invoke against. It
   will use the invoke-sub! function in simple-render-fn. To change this make a new function."
  ([sub-fn] (use-sub sub-fn {}))
  ([sub-fn-or-sub {:keys [lazy? appish initial-default-value]
                   :or {lazy? false initial-default-value nil}}]
   (let [[value set-v!] (hooks/use-state initial-default-value)]
     ; use a nil signal. This should never run again during the course of the component....
     ; or should it? Maybe the signal can be the sub fn? Dynamic subs? Will address later...
     (hooks/use-effect (fn setup-effect* []
                         (let [sub-fn (if lazy? (sub-fn-or-sub) sub-fn-or-sub)
                               sub-map {:calc-fn sub-fn
                                        :set-fn! set-v!}]
                           (when appish
                             (let [app (comp/any->app appish)
                                   app-db (application/current-state app)]
                               (sfr/invoke-sub! app-db sub-map)))
                           (sub-reg/register-sub-fn! sub-map)
                           (fn teardown-effect* []
                             (sub-reg/deregister-sub-fn! sub-map))))
       #js [0])
     value)))