(ns def.freeformsoftware.fulcro-subscriptions.test-hooks-ws
  (:require
    ["react" :refer [createElement]]
    [nubank.workspaces.card-types.fulcro3 :as ct.fulcro]
    [nubank.workspaces.core :as ws]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.application :as app]
    [com.fulcrologic.fulcro.rendering.keyframe-render2 :as kr2]
    [com.fulcrologic.fulcro.dom :as dom :refer [div button p ul]]
    [com.fulcrologic.fulcro.mutations :as m :refer [defmutation]]
    [cljs.test :as test :refer [is]]
    [com.fulcrologic.fulcro.algorithms.merge :as merge]
    [com.fulcrologic.fulcro.react.hooks :as hooks]
    [taoensso.timbre :as log]
    [clojure.set :as set]



    [dev.freeformsoftware.fulcro-subscriptions.subscription :as sub]
    [dev.freeformsoftware.fulcro-subscriptions.simple-render-fn :as render]
    [dev.freeformsoftware.fulcro-subscriptions.subscription-register :as sub-reg]
    [dev.freeformsoftware.fulcro-subscriptions.hooks :as reg-hooks]))


(defsc DemoCard [this props]
  {}
  (dom/h2
    "Hi"))


(ws/defcard demo-card
  (ct.fulcro/fulcro-card
    {::ct.fulcro/wrap-root? false
     ::ct.fulcro/root       DemoCard
     ::ct.fulcro/app        {}}))







(defmutation add-number [_]
  (action [{:keys [state]}]
    (swap! state update :numbers conj (int (rand 400)))))
(defmutation remove-number [_]
  (action [{:keys [state]}]
    (swap! state update :numbers butlast)))

(defn count-of-numbers [{::sub/keys [app-db] :as all}]
  (js/console.log "Just ran " app-db)
  (let [ns (:numbers app-db)]
    {:res (count ns)}))

(defsc SimpleSubs [this {:keys [numbers] :as props}]
  {:query      [:numbers]
   :use-hooks? true}
  (let [count-of-numbers (reg-hooks/use-sub count-of-numbers {:appish this})]
    (dom/div
      (dom/button {:onClick (fn [_] (comp/transact! this [(add-number {})]))}
        "Add number")
      (dom/button {:onClick (fn [_] (comp/transact! this [(remove-number {})]))}
        "Remove number")
      (dom/ul
        (map #(dom/li %) numbers))
      (dom/div (str "Count: " count-of-numbers)))))


(ws/defcard simple-subs
  (ct.fulcro/fulcro-card
    {::ct.fulcro/app        {:optimized-render! (render/wrap-render! kr2/render!)}
     ::ct.fulcro/root       SimpleSubs
     ::ct.fulcro/wrap-root? false
     ::ct.fulcro/initial-state {:numbers [1 2 3 4]}}))






