(ns dev.freeformsoftware.fulcro-subscriptions.dependencies.deps-sorted-test
  (:require [clojure.test :refer :all]
            [dev.freeformsoftware.fulcro-subscriptions.dependencies.deps-sorted :refer :all]
            [dev.freeformsoftware.metacomet.testing-utils :as mc.tu]
            [com.stuartsierra.dependency :as dep]))

;       :a
;    /   |   \
; :b     :c -> :d
;  \     |     /
;   :e   :f   :g
;         |   /
;        :h

(def graph (reduce (fn [graph [x y]] (dep/depend graph x y))
             (dep/graph)
             (partition 2 [:b :a        ;; :b depends on :a
                           :c :a
                           :c :d
                           :d :a
                           :e :b
                           :f :c
                           :g :d
                           :h :f
                           :h :g
                           ])))

(comment
  (:dependencies graph)
  {:b #{:a}, :c #{:d :a}, :d #{:a}, :e #{:b}, :f #{:c}, :g #{:d}, :h #{:g :f}}

  (full-dependencies-set graph #{:h :d}))

(defn check-is-opt [requirements maybe-multiple-deps]
  (let [mdeps (if (vector? maybe-multiple-deps) #{maybe-multiple-deps}
                                                maybe-multiple-deps)]
    (some #(= % (full-dependencies-set graph requirements)) mdeps)))

(deftest test-transitive-sorted
  (testing "graph 1"
    (are [req maybe-mult-deps] (check-is-opt req maybe-mult-deps)
                               #{:b} [:a :b]
                               #{:e} [:a :b :e]
                               #{:c} [:a :d :c]
                               #{:f} [:a :d :c :f]
                               #{:g} [:a :d :g]
                               #{:g :d} [:a :d :g]
                               #{:f :g} #{[:a :d :c :g :f]
                                          [:a :d :c :f :g]} ; just cuz sets are random ordered. Still works.
                               #{:g :f} #{[:a :d :c :g :f]
                                          [:a :d :c :f :g]}
                               #{:h :d} #{[:a :d :c :f :g :h]
                                          [:a :d :c :f :h :g]}
                               #{:d :h} #{[:a :d :c :f :g :h]
                                          [:a :d :c :f :h :g]}
                               )))



