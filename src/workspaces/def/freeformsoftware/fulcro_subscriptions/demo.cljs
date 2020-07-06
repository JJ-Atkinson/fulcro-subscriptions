(ns def.freeformsoftware.fulcro-subscriptions.demo
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
    [clojure.set :as set]))
 