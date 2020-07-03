(ns user
  (:require [clojure.tools.namespace.repl :as tools-ns :refer [set-refresh-dirs]]
            [kaocha.repl :as krpl]
            [kaocha.watch :as kwatch]))




(defn run-tests
  ([] (krpl/run :unit)))

(defn refresh-and-test []
  (tools-ns/refresh)
  (run-tests))

(defn start-watch []
  (kwatch/run (krpl/config)))

