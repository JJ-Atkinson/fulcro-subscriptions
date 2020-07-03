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
  ([] (real-dedupe #{}))
  ([ignore]
   (fn [rf]
     (let [pv (volatile! (or ignore #{}))]
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
  ([ignore coll] (sequence (real-dedupe ignore) coll)))


(>defn transitive-sorted
  "Stolen and modified from the original source. No really an end user product.
   If you pass a set of pre-provided keys, neither they nor any key they depend on will
   be returned, allowing you to skip part of the tree."
  ([neighbors node-set]
   [map? seqable? => seqable?]
   (transitive-sorted neighbors node-set #{}))
  ([neighbors node-set pre-provided]
   [map? seqable? seqable? => seqable?]
   (loop [unexpanded (set (mapcat neighbors node-set))
          ordered-expanded (seq node-set)]
     (if-let [[node & more] (seq (set/difference (set unexpanded) pre-provided))]
       (let [more (dedupe more)]
         (recur (concat more (neighbors node))
           (cons node ordered-expanded)))
       (real-dedupe #{} ordered-expanded)))))



(defn full-dependencies-set
  "Returns dependencies in root first order.
   i.e. C depends on B depends on A,
   (transitive-dependencies-set ... C) => '(A B C), because first you must resolve A, then B, then C"
  ([map-dependency-graph node-set]
   (transitive-sorted (:dependencies map-dependency-graph) node-set))
  ([map-dependency-graph node-set pre-provided]
   (transitive-sorted (:dependencies map-dependency-graph) node-set pre-provided)))

