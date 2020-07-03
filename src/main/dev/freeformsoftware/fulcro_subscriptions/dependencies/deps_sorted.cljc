(ns dev.freeformsoftware.fulcro-subscriptions.dependencies.deps-sorted
  (:require [com.fulcrologic.guardrails.core :refer [>defn => >def]]
            [clojure.spec.alpha :as s]
            [com.stuartsierra.dependency :as dep]
            [clojure.set :as set]))


;; this is here because dependency/transitive-dependencies returns an unsorted set. 
;; this implementation retains ordering requirements (i.e. a <dep- b <dep- c,
;; (dependencies c) -> [a b], meaning resolve a before b).

(->
  (dep/graph)
  (dep/depend :b :a)
  (dep/depend :a :c)
  ;(dep/transitive-dependencies :b)
  ;:dependencies
  ;:dependents
  )

(defn real-dedupe
  "Returns a lazy sequence removing duplicates in coll.
  Returns a transducer when no collection is provided."
  ([]
   (fn [rf]
     (let [pv (volatile! #{})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [in? (contains? @pv input)]
            (when-not in?
              (vreset! pv (conj @pv input)))
            (if in?
              result
              (rf result input))))))))
  ([coll] (sequence (real-dedupe) coll)))


(>defn transitive-sorted
  "Stolen and modified from the original source. No really an end user product - prefer"
  [neighbors node-set]
  [map? (s/coll-of keyword?) => (s/coll-of keyword?)]
  (loop [unexpanded (set (mapcat neighbors node-set))
         ordered-expanded (seq node-set)]
    (if-let [[node & more] (seq unexpanded)]
      (let [more (dedupe more)]
        (recur (concat more (neighbors node))
          (cons node ordered-expanded)))
      (real-dedupe ordered-expanded))))


(defn full-dependencies-set
  "Returns dependencies in root first order.
   i.e. C depends on B depends on A,
   (transitive-dependencies-set ... C) => '(A B C), because first you must resolve A, then B, then C"
  [map-dependency-graph node-set]
  (transitive-sorted (:dependencies map-dependency-graph) node-set))

