(ns dev.freeformsoftware.fulcro-subscriptions.subscription-register-test
  (:require [clojure.test :refer :all :as t]
            [dev.freeformsoftware.fulcro-subscriptions.subscription-register :refer :all]
            [dev.freeformsoftware.metacomet.testing-utils :as mc.tu]
            [dev.freeformsoftware.metacomet.prim.seq-utils :as mc.su]))

(deftest test-register-sub-fn
  (testing "Invoke-fn works"
    (let [[count f] (mc.tu/counting)]
      (register-sub-fn! f 1)
      (is (= @count 1))))
  (testing "Register works"
    (register-sub-fn! 2)
    (is (mc.su/in? (all-subs) 2))
    (deregister-sub-fn! 2)
    (is (not (mc.su/in? (all-subs) 2)))))