;a test.
(ns nathansorenson.AWTExceptionHandler
	(:import [nathansorenson LevelStub])
	(:require
	 [nathansorenson.ga :as ga]
	 [nathansorenson.level-generator :as lge]
	 [nathansorenson.leveleval :as lev])
	(:gen-class
	 :methods [[getString [] String]
						 [echo [String] String]
						 [randLevel [] nathansorenson.LevelStub]]))

(defn -handle [#^Throwable t]
	(try "Glarg!"
			 (catch Throwable t "Blarg!")))

(defn -getString [this]
	(String. "hey"))

(defn -echo [this string]
	(String. string))

(defn -randLevel [this]
	(lge/generate-level (lev/constrain-genotype (:genotype (ga/rand-individual)))))
