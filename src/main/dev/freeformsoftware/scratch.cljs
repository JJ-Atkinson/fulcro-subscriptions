(ns dev.freeformsoftware.scratch)

(ns com.fulcrologic.fulcro.cards.reducer4-cards
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
    [clojure.set :as set])
  )

(declare Customer Sale)
(def names ["Sam" "Joe" "CoolDude" "James" "AnotherDude" "Name"])
(defonce SPA (atom nil))
(defn state [] (app/current-state @SPA))

(defonce subscription-fns (atom #{}))


;; helpers for me
(defn ident->id [ident]
  (assert (and (vector? ident) (= (count ident) 2)))
  (second ident))

(defn ident->table-id [ident]
  (assert (and (vector? ident) (= (count ident) 2)))
  (first ident))

;; this is part of the call-subs step

(defn invoke-sub [app-db {:as sub-map :keys [calc-fn set-fn]}]
  (let [{:keys [unchanged? res]} (calc-fn app-db)]
    (when-not unchanged? (set-fn res))))

;; this is a step

(defn register-sub-fn!
  "Accepts a sub map in the form
   {:calc-fn         (fn [app-db] {:unchanged? boolean :res value)
    :set-fn          (fn [res] ...)}
   
   Calc fn is usually created using some of the below methods, but could be as simple
   as some inline function that depends on app-db, or something as complicated
   as a partially memoized pathom based setup. Below in code is one possibility.
   
   
   It will only call the :set-fn (based on react hooks?) if :unchanged? is false."
  [sub-map]
  (swap! subscription-fns conj sub-map)
  (invoke-sub (state) sub-map))

(defn de-register-sub-fn!
  [sub-map]
  (swap! subscription-fns disj sub-map))


;; this renderer is a step


(defn call-subs [app]
  (log/info "Calling subscriptions. Count of them:" (count @subscription-fns))
  (let [app-db (app/current-state app)]
    (doall (map #(invoke-sub app-db %) @subscription-fns))))


;; this is a step 

(defn render!
  "Custom renderer that uses normal kf2/render!, but also sends updates to query subscribers."
  [app options]
  (log/info "Calling custom render reducer4")
  (call-subs app)
  (kr2/render! app options))




;; normal functions that work on the db here... Not good for memoization, but great for
;; instance-sub-fast? doing a diff check.

(defn customers-extractor [app-db]
  (get app-db :customer/id))

(defn sales-extractor [app-db]
  (get app-db :sale/id))


;; normal functions that work out of the context of fulcro.

(defn sum-sale-price [sales]
  (reduce + 0 (map :sale/amount sales)))

(defn customer-count [customers]
  (count customers))

(defn all-sales [sales-table]
  (vals sales-table))

(defn customer-extractor [customers-table customer-id]
  (get customers-table customer-id))

(defn customer-sales [sale-table customer]
  (map #(get sale-table (ident->id %)) (:customer/purchases customer)))



;; this is not at all tied to the subscription system. Could be fully implemented without
;; this function. But, this turns out to be useful if you are doing intermediate changed?
;; calculations on the args between the functions. Also, gives you some separation from implementation.
;; given a global register of these calls, a dependency resolution algorightim could sort this for
;; arbitrary runtime calls... If made into a macro, those dependencies could be found at runtime.
;; Since the mechanism is so transparent, it wouldn't really need much in the way of tooling
;; for introspection like pathom does.
(defn lift-to-subscription [f input-vector output]
  {:f        (fn lift-fn* [input-map]
               (apply f (map input-map input-vector)))
   :requires input-vector
   :output   output})




;; all of these functions are just functions for the most part. some api could put them
;; all in a memoization pool, maybe part of the fulcro app db itself...

(fn memoize [f atom table-id] #_...)

;; this depends on the type

(def sum-all-sales<
  [(lift-to-subscription sales-extractor [:app-db] :sales-table)
   (lift-to-subscription all-sales [:sales-table] :sales)
   (lift-to-subscription sum-sale-price [:sales] :sum)])


(defn sum-customer-sales< [customer-id]
  [(lift-to-subscription customers-extractor [:app-db] :customers)
   (lift-to-subscription #(customer-extractor % customer-id) [:customers] :customer)
   (lift-to-subscription sales-extractor [:app-db] :sales-table)
   (lift-to-subscription customer-sales [:sales-table :customer] :customer-sales)
   (lift-to-subscription sum-sale-price [:customer-sales] :sum)])


(def customer-count<
  [(lift-to-subscription customers-extractor [:app-db] :customers)
   (lift-to-subscription customer-count [:customers] :count)])



;; first way this could be implemented... Just makes sure while reducing that if it
;; can, it will short circuit. for my case this is useful iff memoization is not implemented
;; at the function level (^^ see above)
(defn instance-sub-fast? [sub-vector]
  (assert (vector? sub-vector))
  (let [prior-run-result (atom {})]
    (fn sub-runner* [app-db]
      (loop [[current-sub & rest] sub-vector
             run-result (assoc @prior-run-result
                          :app-db app-db)
             unchanged-set #{}]
        (let [args (set (:requires current-sub))
              old-res (-> current-sub :output run-result)
              unchanged? (set/subset? args unchanged-set)
              new-res (if unchanged? old-res ((:f current-sub) run-result))
              unchanged? (or unchanged? (= new-res old-res))
              run-result (assoc run-result
                           (:output current-sub)
                           new-res)
              unchanged-set (cond-> unchanged-set
                              unchanged? (conj (:output current-sub)))]
          (if-not (empty? rest)
            (recur rest run-result unchanged-set)
            (do
              (reset! prior-run-result run-result)
              {:res new-res :unchanged? unchanged?})))))))


;; this ignores the possible efficiency gain above, but retains the same api. Good for really cheap
;; subscriptions. idk if the final value is checked for change inside react, so maybe this impl
;; should maintain a ref to the prior value at least, but not the entire intermediate tree.
(defn instance-sub-slow? [sub-vector]
  (assert (vector? sub-vector))
  (fn sub-runner* [app-db]
    (loop [[current-sub & rest] sub-vector
           run-result {:app-db app-db}]
      (let [new-res ((:f current-sub) run-result)
            run-result (assoc run-result
                         (:output current-sub)
                         new-res)]
        (println run-result)
        (if-not (empty? rest)
          (recur rest run-result)
          {:res new-res :unchanged? false})))))

(declare app-db*)
(comment

  (def app-db* @(state))
  (keys app-db*)

  (def sub1 (instance-sub-fast? sum-all-sales<))
  (def sub1 (instance-sub-slow? sum-all-sales<))
  (def sub2 (instance-sub-slow? customer-count<))
  (sub1 @(state))
  (sub1 (app/current-state @SPA))

  (let [[a b c] sum-all-sales<
        init {:app-db app-db*}
        next (assoc init (:output a) ((:f a) init))
        final (assoc next (:output b) ((:f b) next))]
    ((:f c) final)))


(defn use-sub
  "Sub-fns are potentially expensive things to create, so you can use the lazy? flag to indicate
   that you are passing a no arg function that creates the sub."
  ([sub-fn] (use-sub sub-fn false))
  ([sub-fn-or-sub lazy?]
   (let [[value set-v!] (hooks/use-state 1)]
     ; use a nil signal. This should never run again during the course of the component....
     ; or should it? Maybe the signal should be the sub fn? Will address later...
     (hooks/use-effect (fn *setup-effect []
                         (log/info "Setting up sub!")
                         (let [sub-fn (if lazy? (sub-fn-or-sub) sub-fn-or-sub)
                               sub-map {:calc-fn sub-fn
                                        :set-fn  set-v!}]
                           (register-sub-fn! sub-map)
                           (fn *teardown-effect []
                             (log/info "Killing sub!")
                             (de-register-sub-fn! sub-map))))
       #js [0])
     value)))



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
  (let [sales-total (use-sub (fn []
                               (instance-sub-slow? (sum-customer-sales< id)))
                      true)]
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
  (let [sum (use-sub (instance-sub-fast? sum-all-sales<))
        customer-count (use-sub (instance-sub-fast? customer-count<))]

    (dom/div
      (dom/button
        {:onClick (fn [e]
                    (comp/transact! this [(add-cust {})]))}
        "+ Add Customer")
      (dom/h2 (str "Number of customers " customer-count))
      (dom/h2 (str "Sum of customer sales: " sum))

      (mapv ui-customer customers))))


(def ui-top (comp/factory Top {:keyfn :top/id}))




(ws/defcard reducer4-card
  (ct.fulcro/fulcro-card
    {::ct.fulcro/wrap-root? true
     ::ct.fulcro/root       Top
     ;; NOTE: Just invented tx-hook...simple add to tx-processing

     ::ct.fulcro/app        {:optimized-render! render!}
     }))



