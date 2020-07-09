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
    {::ct.fulcro/app           {:optimized-render! (render/wrap-render! kr2/render!)}
     ::ct.fulcro/root          SimpleSubs
     ::ct.fulcro/wrap-root?    false
     ::ct.fulcro/initial-state {:numbers [1 2 3 4]}}))






(declare Customer Sale)
(def names ["Sam" "Joe" "CoolDude" "James" "AnotherDude" "Name"])
(defonce SPA (atom nil))
(defn state [] (app/current-state @SPA))




(defn gen-sale []
  #:sale{:id     (rand-int 99999999)
         :amount (rand-int 80)})

(defn gen-customer []
  #:customer{:id        (rand-int 99999999)
             :name      (rand-nth names)
             :purchases [(gen-sale)
                         (gen-sale)]})


(defmutation add-sale [{:keys [cust-id]}]
  (action [{:keys [state]}]
    (swap! state merge/merge-component Sale (gen-sale)
      :append [:customer/id cust-id :customer/purchases])))


(defmutation add-cust [{:keys []}]
  (action [{:keys [state]}]
    (swap! state merge/merge-component Customer (gen-customer)
      :append [:top/id 1 :top/customers])))

(defmutation remove-cust [{:keys [cust-id]}]
  (action [{:keys [state]}]
    (swap! state #(-> % (merge/remove-ident* [:customer/id cust-id]
                          [:top/id 1 :top/customers])
                    (update-in [:customer/id] dissoc cust-id)))))


(defn sum-sales-list [sales-list]
  (reduce (fn [acc s] (+ acc (:sale/amount s)))
    0
    sales-list))


(sub/defprovider all-sales [adb]
  {::sub/providers [::sub/app-db]}
  (:sale/id adb))

(sub/defprovider all-customers [adb]
  {::sub/providers [::sub/app-db]}
  (:customer/id adb))

(sub/defprovider customer-count [customer-list]
  {::sub/providers [all-customers]}
  (count customer-list))


(sub/defprovider sum-all-sales [sales]
  {::sub/providers [all-sales]}
  (sum-sales-list (vals sales)))

(sub/defprovider sum-self-sales [sales-table customer-table cust-id]
  {::sub/providers [all-sales all-customers :customer/id]
   ::sub/params    #{:customer/id}}
  (let [cust-purch (get-in customer-table [cust-id :customer/purchases])
        sales (map #(get sales-table (second %)) cust-purch)]
    (sum-sales-list sales)))



(defsc Sale [this {:keys [sale/id sale/amount] :as props}]
  {:query         [:sale/id
                   :sale/amount]
   :ident         :sale/id
   :initial-state #:sale{:id     :param/id
                         :amount :param/amount}}
  (str "Sale: $" amount))

(def ui-sale (comp/factory Sale {:keyfn :sale/id}))





(defsc Customer [this {:customer/keys [id name purchases] :as props}]
  {:query      [:customer/id
                :customer/name
                {:customer/purchases (comp/get-query Sale)}]
   :ident      :customer/id
   :use-hooks? true}
  (let [sales-total (sub/subscribe! this sum-self-sales {:customer/id id})]
    (dom/div
      (dom/h4 name)
      (dom/button
        {:onClick (fn [e] (comp/transact! this [(add-sale {:cust-id id})]))}
        "+ Add Sale")
      (dom/button
        {:onClick (fn [e] (comp/transact! this [(remove-cust {:cust-id id})]))}
        "- Remove Self")
      (dom/div (str "sum of my sales " sales-total))
      (dom/ul
        (mapv #(dom/li (ui-sale %)) purchases))
      (dom/hr))))

(def ui-customer (comp/factory Customer {:keyfn :customer/id}))

(((::sub/invocation-strategy sum-self-sales)
  @(::sub/dependency-chain sum-self-sales)) {::sub/app-db {:customer/id {1 1 2 2 3 3 4 4}}})
;(sub/chain-descriptions `customer-count)
;
;
(defsc Top [this {:keys [top/id top/customers] :as props}]
  {:query         [:top/id
                   {:top/customers (comp/get-query Customer)}]
   :ident         :top/id
   :initial-state (fn [_] #:top{:id        1
                                :customers [(gen-customer)
                                            (gen-customer)]})
   :use-hooks?    true}
  ;; in here so I have access to it asap. :client-did-mount is called after first render
  ;; afaict
  (reset! SPA (comp/any->app this))
  (let [sum (sub/subscribe! this sum-all-sales)
        customer-count (sub/subscribe! this customer-count)]

    (dom/div
      (dom/button
        {:onClick (fn [e]
                    (comp/transact! this [(add-cust {})]))}
        "+ Add Customer")
      (dom/h2 (str "Number of customers " customer-count))
      (dom/h2 (str "Sum of customer sales: " sum))

      (mapv ui-customer customers))))


(def ui-top (comp/factory Top {:keyfn :top/id}))




(ws/defcard high-level-subscriptions
  (ct.fulcro/fulcro-card
    {::ct.fulcro/wrap-root? true
     ::ct.fulcro/root       Top
     ;; NOTE: Just invented tx-hook...simple add to tx-processing

     ::ct.fulcro/app        {:optimized-render! (render/wrap-render! kr2/render!)}}))



