(ns dev.freeformsoftware.fulcro-subscriptions.simple-render-fn
  (:require [dev.freeformsoftware.fulcro-subscriptions.subscription-register :as reg]
            [com.fulcrologic.fulcro.application :as app]))


(defn invoke-sub! [app-db {:as sub-map :keys [calc-fn set-fn!]}]
  (let [{:keys [unchanged? res]} 
        (calc-fn {:dev.freeformsoftware.fulcro-subscriptions.subscription/app-db app-db})]
    (when-not unchanged? (set-fn! res))))


(defn invoke-all-subs! 
  ([fulcro-app] (invoke-all-subs! fulcro-app (reg/all-subs)))
  ([fulcro-app subscription-seq]
   (let [adb (app/current-state fulcro-app)]
     (println "Calling all subs!" (count subscription-seq) (keys adb))
     (doseq [sub subscription-seq]
       (invoke-sub! adb sub)))))


(defn wrap-render!
  ""
  ([renderer]
   (wrap-render! renderer {}))
  ([renderer {:keys []}]
   (fn [app options]
     (renderer app options)
     (println "Calling all subs!" )
     (invoke-all-subs! app))))