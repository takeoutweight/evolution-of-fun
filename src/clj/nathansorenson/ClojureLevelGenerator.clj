(ns nathansorenson.ClojureLevelGenerator
  (:import [nathansorenson LevelStub]
           [dk.itu.mario.MarioInterface Constraints])
  (:require
   [nathansorenson.level-evolve :as levo]
   [nathansorenson.level-generator :as lg])
  (:gen-class
   :methods [[randLevel [] nathansorenson.LevelStub]]))

(defn -randLevel [this]
  (prn "Starting level evolution for 40 seconds...")
  (levo/start)
  (Thread/sleep (* 45 1000))
  (levo/stop)
  (lg/generate-level (:genotype @levo/current-best-indiv) Constraints/levelWidth))
