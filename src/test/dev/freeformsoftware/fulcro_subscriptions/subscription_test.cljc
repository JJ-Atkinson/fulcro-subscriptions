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
(add-default-subs!)

(register-subscription-description
  (describe-function + [::dut/app-db] :a))
(register-subscription-description
  (describe-function + [:a] :b))
(register-subscription-description
  (describe-function + [:a] :aa))
(register-subscription-description
  (describe-function + [:a :b ::dut/app-db] :c))
(register-subscription-description
  (describe-function + [:b :aa] :cc))

;;        dut/app-db
;;        /        \
;;       |   -------a
;;       |  /      /  \
;;       | |  ---b     aa
;;       \ | /    \     /
;;         c         cc

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