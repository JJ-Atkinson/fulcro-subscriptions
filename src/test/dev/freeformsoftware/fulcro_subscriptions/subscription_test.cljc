(ns dev.freeformsoftware.fulcro-subscriptions.subscription-test
  (:require [clojure.test :refer :all]
            [dev.freeformsoftware.fulcro-subscriptions.subscription :refer :all]
            [dev.freeformsoftware.metacomet.testing-utils :as mc.tu]))


(def fadd (describe-function + [:a :b] :d))
(def fstr (describe-function str [:a :b] :d))

(deftest test-describe-function
  (testing "meets specs"
    (is (describe-function (fn []) [:coll-of-keyword] :keyword))))


(deftest test-invoke-subscription-definition
  (testing "correctly invokes the fn"
    (are [m desc res] (= (invoke-subscription-definition desc m) res)
                      {:a 2 :b 2} fadd 4
                      {:a "a"} fstr "a"
                      nil fstr "")))

(deftest test-apply-description-definition
  (testing "correctly invokes and adds the fn"
    (are [m desc res] (= (apply-description-definition desc m) (assoc m :d res))
                      {:a 2 :b 2} fadd 4
                      {:a "a"} fstr "a"
                      nil fstr "")))



