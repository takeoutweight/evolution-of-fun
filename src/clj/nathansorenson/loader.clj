(ns nathansorenson.loader
  (:refer-clojure))
  ;(:use [clojure.contrib.stacktrace :as st]))

(def root "./")

(defn reload []
  
  
  (do
    (load-file (str root "src/le/loader.clj"))))

;seems to load based on root of project now,
(defn load-full []
  (do
    (load-file (str root "src/le/nutils.clj"))
    (load-file (str root "src/le/level-generator.clj"))
    (load-file (str root "src/le/flexencoding.clj"))
    (load-file (str root "src/le/fileio.clj"))
    (load-file (str root "src/le/leveleval.clj"))
    (load-file (str root "src/le/imageutils.clj"))
    (load-file (str root "src/le/visualization.clj"))
    (load-file (str root "src/le/levelreader.clj"))
;CAUTION!: works via clobbering!
;(load-file "zeldaencoding.clj")
    (load-file (str root "src/le/ga.clj"))
    (load-file (str root "src/le/window.clj"))))

(defn load-slim []
  (do
    (load-file (str root "src/le/nutils.clj"))
    (load-file (str root "src/le/level-generator.clj"))
    (load-file (str root "src/le/flexencoding.clj"))
    (load-file (str root "src/le/leveleval.clj"))
    (load-file (str root "src/le/ga.clj"))
    ))

;NOTES:
;ideal level about 300*64 = 19200
;blocks are in sprite tags, with <Property name="type" value="massive"?

(load-file (str root "src/le/nutils.clj"))
(load-file (str root "src/le/level-generator.clj"))
(load-file (str root "src/le/flexencoding.clj"))
(load-file (str root "src/le/leveleval.clj"))
(load-file (str root "src/le/imageutils.clj"))
(load-file (str root "src/le/levelreader.clj"))
(load-file (str root "src/le/ga.clj"))
(load-file (str root "src/le/gamma-model.clj"))
;(load-file (str root "src/le/gamma-level.clj"))
