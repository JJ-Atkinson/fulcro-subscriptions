(ns dev.freeformsoftware.fulcro-subscriptions.subscription-test
  (:require [clojure.test :refer :all]
            [dev.freeformsoftware.fulcro-subscriptions.subscription :as dut :refer :all]
            [dev.freeformsoftware.metacomet.testing-utils :as mc.tu]))


(def fadd (describe-function + [:a :b] :d))
(def faddsym (describe-function + ['a 'b] 'd))
(def fstr (describe-function str ['a 'b] 'd))

(deftest test-describe-function
  (testing "meets specs"
    (is (describe-function (fn []) [:coll-of-keyword] :keyword))))


(deftest test-invoke-subscription-definition
  (testing "correctly invokes the fn"
    (are [m desc res] (= (invoke-subscription-definition desc m) res)
                      {:a 2 :b 2} fadd 4
                      {'a "a"} fstr "a"
                      nil fstr "")))

(deftest test-apply-description-definition
  (testing "correctly invokes and adds the fn"
    (are [m desc res] (= (apply-description-definition desc m) (assoc m 'd res))
                      {'a 2 'b 2} faddsym 4
                      {'a "a"} fstr "a"
                      nil fstr "")))

(reset-subscription-description-register!)


(register-subscription-description
  (describe-function str [::dut/app-db] :a))
(register-subscription-description
  (describe-function str [:a] :b))
(register-subscription-description
  (describe-function str [:a] :aa))
(register-subscription-description
  (describe-function str [::dut/app-db :a :b] :c))
(register-subscription-description
  (describe-function str [:aa :b] :cc))

(register-subscription-description
  (describe-function pos? [::dut/app-db] :pos?))
(register-subscription-description
  (describe-function str [:pos?] :pos-str))

;;        dut/app-db --------
;;        /        \         \
;;       |   -------a        pos?
;;       |  /      /  \        \
;;       | |  ---b     aa      pos-str
;;       \ | /    \     /
;;         c        cc

(deftest test-subscription-def-register
  (testing "can correctly resolve the fact that ::app-db is available"
    (is (= (count (chain-descriptions :a))
          2)))
  (testing "that it correctly adds dependencies to the graph"
    (is (= (count (chain-descriptions :b))
          3))
    (is (= (count (chain-descriptions :c))
          4))
    (is (= (count (chain-descriptions :cc))
          5))))


(deftest test-simple-invocation-strategy
  (testing "That the results generated are expected"
     (let [strat (fn [goal prov] (-> (chain-descriptions goal prov) (simple-invocation-strategy)))]
      (is (= ((strat :a #{::dut/app-db}) {::dut/app-db "appdb"})
            {:res "appdb" :unchanged? false}))
      (is (= ((strat :c #{::dut/app-db}) {::dut/app-db "appdb"})
            ;; appdb    a    b
            {:res "appdbappdbappdb" :unchanged? false}))
      (is (= ((strat :c #{::dut/app-db :a}) {::dut/app-db "appdb" :a "A"})
            ;; appdb    ab
            {:res "appdbAA" :unchanged? false}))
      (is (= ((strat :c #{::dut/app-db :a}) {::dut/app-db "appdb"
                                             :a "A" :b "B"})
            ;; this works because if you pass addl args that you don't specify beforehand
            ;; they might get stomped on.
            ;; appdb    ab
            {:res "appdbAA" :unchanged? false}))
      (is (= ((strat :c #{::dut/app-db :a :b}) {::dut/app-db "appdb"
                                                :a "A" :b "B"})
            ;; appdb    ab
            {:res "appdbAB" :unchanged? false})))))


(let [strat (fn [goal prov] (-> (chain-descriptions goal prov) (short-circuit-invocation-strategy)))]
  (deftest test-short-circuit-invocation-strategy
    (testing "has the same results as the simple invocation"
      (is (= ((strat :a #{::dut/app-db}) {::dut/app-db "appdb"})
               {:res "appdb" :unchanged? false}))
      (is (= ((strat :c #{::dut/app-db}) {::dut/app-db "appdb"})
            ;; appdb    a    b
            {:res "appdbappdbappdb" :unchanged? false}))
      (is (= ((strat :c #{::dut/app-db :a}) {::dut/app-db "appdb" :a "A"})
            ;; appdb    ab
            {:res "appdbAA" :unchanged? false}))
      (is (= ((strat :c #{::dut/app-db :a}) {::dut/app-db "appdb"
                                             :a           "A" :b "B"})
            ;; this works because if you pass addl args that you don't specify beforehand
            ;; they might get stomped on.
            ;; appdb    ab
            {:res "appdbAA" :unchanged? false}))
      (is (= ((strat :c #{::dut/app-db :a :b}) {::dut/app-db "appdb"
                                                :a           "A" :b "B"})
            ;; appdb    ab
            {:res "appdbAB" :unchanged? false})))
    (testing "unchanged? should work."
      (let [str-a (strat :pos-str #{::dut/app-db})]
        (is (= (str-a {::dut/app-db 1}) {:res "true" :unchanged? false}))
        (is (= (str-a {::dut/app-db 1}) {:res "true" :unchanged? true}))
        (is (= (str-a {::dut/app-db 2}) {:res "true" :unchanged? true}))
        (is (= (str-a {::dut/app-db -1}) {:res "false" :unchanged? false}))
        (is (= (str-a {::dut/app-db -19}) {:res "false" :unchanged? true}))))))
