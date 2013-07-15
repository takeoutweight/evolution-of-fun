
(ns nathansorenson.level-generator
  (:use [nathansorenson.nutils])
  (import (java.util Random)
;         (ch.idsia.mario.engine.sprites Enemy)
          (dk.itu.mario.engine.sprites SpriteTemplate)
;         (ch.idsia.scenarios ClojureStub)
;         (ch.idsia.mario.engine Art LevelRenderer)
          (nathansorenson LevelStub)))

; length is classically 2048 ?!? (really?) height 15.
;"height" of floor is between - [h - 4 .. height-1] so high y is a low altitude.
;a gap can be AT MOST 10 units long. And that's an extremely difficult jump.
;enemies 0-3 are dudes, 4 is the flower guy. (pirhana plant?)

;http://dev.clojure.org/jira/browse/CLJ-441
(def bground (unchecked-byte (+ 1 (* 9 16))))
(def bstair (byte (+ 9 (* 0 16))))
(def bpipetr (byte (+ 10 (* 0 16))))
(def bpipetl (byte (+ 11 (* 0 16))))
(def bpipebr (byte (+ 10 (* 1 16))))
(def bpipebl (byte (+ 11 (* 1 16))))
(def bcannt (byte (+ 14 (* 0 16))))
(def bcannm (byte (+ 14 (* 1 16))))
(def bcannb (byte (+ 14 (* 2 16))))
(def bblock [(byte (+ 6 (* 1 16))) ;0 flower
             (byte (+ 5 (* 1 16))) ;1 coin
             (byte (+ 2 (* 1 16))) ;2 secret flow
             (byte (+ 1 (* 1 16))) ;3 secret coin
             (byte (+ 0 (* 1 16)))]) ;4 regular
(def coin-block (byte (+ 8 (* 1 16))))
(def brock (byte 9))
(def bcoin (byte (+ 2 (* 2 16))))
(def bhillf (int (+ 5 (* 9 16))))
(def bhilll (int (+ 4 (* 9 16))))
(def bhillr (int (+ 6 (* 9 16))))
(def bhillt (int (+ 5 (* 8 16))))
(def bhilltl (int (+ 4 (* 8 16))))
(def bhilltr (int (+ 6 (* 8 16))))
(def bhilltli (int (+ 4 (* 11 16))))
(def bhilltri (int (+ 6 (* 11 16))))


(def bhole 0)

(defn hills!
  "done in one pass, as there are different tilesets for overlapping hills.
   A hill is a {:height :width :x} map."
  [lev genotype]
  (let [hs0 (map #(if (and (= (:type %) :impediment)
                           (or (= (:kind %) :hill1)
                               (= (:kind %) :hill2)))
                    (merge % {:type :hill :height (- 12 2)
                              :width (if (= (:kind %) :hill1) 4 9)})
                    %) genotype)
        hs (sort-by :height (filter #(or
                                      (= (:type %) :hill-with-enemies)
                                      (= (:type %) :hill))
                                    hs0))]
    (doall
     (for [{:keys [height width x]} hs]
             ;top corners
             (do
               (doall
                (map (fn [[ix b1 b2]]
                       (.setBlock lev ix height
                                  (if (= 0 (.getBlock lev ix height)) b1 b2)))
                     [[x bhilltl bhilltli]
                      [(+ x (dec width)) bhilltr bhilltri]]))
             ;top, left, right and centre
               (doall
                (map
                 (fn [[block range]]
                   (doall
                    (map (fn [[ix iy]]
                           (.setBlock lev ix iy block))
                         range)))
                 [[bhillt (map vector (range (inc x) (+ x (dec width)))
                                      (repeat height))]
                  [bhilll (map vector (repeat x)
                                      (range (inc height) 15))]
                  [bhillr (map vector (repeat (+ x (dec width)))
                                      (range (inc height) 15))]
                  [bhillf (for [ix (range (inc x) (+ x (dec width)))
                                iy (range (inc height) 15)] [ix iy])]])))))))

(defn get-height 
  "returns height of first empty space above the ground.
   Default height right now is 12."
  [heightmap x]
  (get heightmap x 12))

(defn reduce*
  "like reduce, but can fold over multiple seqs with a multi-parameter fn
   works the same way map handles multiple seqs.
   TODO: if this proves useful, put it in a util lib."
  [f val & colls]
  (reduce (fn [acc args] (apply f acc args))
            val
            (apply map vector colls)))

(defn heightmap
  "returns an x=>y sorted map of heights for a given level"
  [genotype]
  (let [addrect (fn [hm w h x0]
                  (reduce #(assoc %1 %2 h) hm
                          (for [x (range w)] (+ x x0))))]
    (reduce
     (fn [hm gene]
       (case (:type gene)
         :steps (let [{:keys [height width reverse? num x]} gene
                      stepwidth (int (/ width num))
                      stepheight (int (/ height num))]
                  (reduce* assoc hm 
                           (iterate inc x)
                           ((if reverse? identity reverse)
                            (mapcat (fn [sw sh] (repeat sw sh))
                                   (conj (repeat stepwidth) (max stepwidth 2))
                                   (reverse
                                    (take num (rest (iterate #(- % stepheight) 12))))))))
         :enemy-pit (addrect hm (:width gene) 13 (:x gene))
         :impediment (addrect hm 3 (- 13 7) (+ 8 (:x gene)))
         :hole (addrect hm (:width gene) 14 (:x gene))
         hm))
     (sorted-map)
     genotype)))

;these functions destructively write the element to the level object.
(defn cannon! [level hm x]
  (let [y (get-height hm x)]
      (do
        (.setBlock level x (- y 0) bcannb)
        (.setBlock level x (- y 1) bcannm)
        (.setBlock level x (- y 2) bcannt))))

(defn enemyxy!
  "type: 0 red koopa, 1 green koopa, 2 goomba, 3 spikey, 4 pirhana"
  [level x y type wings?]
  (.setSpriteTemplate level x y (SpriteTemplate. type wings?)))

(defn enemy!
  "type: 0 red koopa, 1 green koopa, 2 goomba, 3 spikey, 4 pirhana"
  [level hm x type wings?]
  (.setSpriteTemplate level x (get-height hm x) (SpriteTemplate. type wings?)))

(defn pipe! [level hm x height piranha?]
  "specified by bottom left square."
  (let [y (get-height hm x)]
    (do 
      (doall (for [iy (range (+ 2 (- y height)) (inc y))]
           (do (.setBlock level x iy bpipebr)
               (.setBlock level (inc x) iy bpipebl))))
      (.setBlock level x (inc (- y height)) bpipetr)
      (.setBlock level (inc x) (inc (- y height)) bpipetl)
      (when piranha?
        (.setSpriteTemplate level x (+ 2 (- y height)) (SpriteTemplate. 4 false))))))

(defn block!
  "type: 0-flower, 1-coin,2-secretflower,3-secretcoin,4 regular"
  [lev hm x type]
  (.setBlock lev x (- (get-height hm x) 3) (bblock type)))

(defn blockxy!
  "set the exact coordinates"
  [lev x y type]
  (.setBlock lev x y (bblock type)))

(defn blocks!
  "should be 4 or 5 above the ground.
   height-4 is too high to get on top of.
   height-3 is just barely low enough to mount."
  [level hm x width coins]
  (let [h (apply min (map #(get-height hm %) (range x (+ x width))))]
  (doall (for [i (range width)]
           (.setBlock level (+ x i) (- h 3) (bblock (rand-elt [0 2 4])))))
  (doall (map #(.setBlock level (+ x %) (- h 3) (bblock 1))
              (take coins (shuffle (range width)))))))

(defn enemy-row-xy!
  "a row of enemies at an arbitrary xy coord"
  [lev x y num kind]
  (doall (for [i (range num)]
           (enemyxy! lev (+ x i) y kind false))))

(defn blocks-with-enemies!
  "should be 4 or 5 above the ground.
   height-4 is too high to get on top of.
   height-3 is just barely low enough to mount."
  [level hm x width num kind coins]
  (let [h (apply min (map #(get-height hm %) (range x (+ x width))))]
    (do
      (doall (for [i (range width)]
               (.setBlock level (+ x i) (- h 3) (bblock (rand-elt [0 2 4])))))
      (doall (map #(.setBlock level (+ x %) (- h 3) (bblock 1))
                  (take coins (shuffle (range width)))))
      (enemy-row-xy! level x (- h 4) (min width num) kind))))

(defn coin-arc!
  [level hm x]
  (let [h (apply min (map #(- (get-height hm %) 2) (range x (+ x 4))))]
    (doall
     (map #(.setBlock level %1 %2 bcoin)
          (iterate inc x)
          [h (dec h) (dec h) h]))))

(defn coin-row!
  [level hm x width]
  (let [h (apply min (map #(- (get-height hm %) 2) (range x (+ x width))))]
    (doall
     (map #(.setBlock level %1 %2 bcoin)
          (iterate inc x)
          (repeat width h)))))


(defn hole! [level x width]
  (doall (for [ix (range x (+ x width)) iy (range 0 15)]
           (case (.getBlock level ix iy)
                 bground (.setBlock level ix iy 0)))))

(defn enemy-pit!
  "TODO: we need to enable alternate enemy-pits, not just ones
   sunk into the earth. To do this, we need a method based on
   the heighmap, and parameters for left and right stoppers.
   QUESITON: why even have this when we have enemy-row???
   A: perhaps the spacing is by default greater?"
  [lev hm x type num]
  (doall (map #(enemy! lev hm % type false)
              (range x (+ x num)))))

(defn enemy-row! [lev hm x type num shell? separation]
  (doall
   (map #(enemy! lev hm % 
                 (if (and shell? (= % x)) 1 type)
                 false)
        (map #(-> % (* separation) (+ x)) (range num)))))

(defn enemy-pit-above!
  "an above-ground enemy pit."
  [lev hm x w type num left right]
  (do (enemy-row! lev hm (+ x 2) type (min num (- w 4)) false 1)
      (doall (map (fn [[type x0]]
             (let [y0 (get-height hm x0)]
               (case type
                   :pipe (pipe! lev hm x0 3 false)
                   :cannon (cannon! lev hm x0)
                   :rock (.setBlock lev x0 y0 brock))))
           [[left x] [right (+ x w -2)]]))))



(defn steppingstone!
  "A way of getting up to a higher level
   Mario can jump over 4 blocks vertically.
   TODO: not sure what to call this construction.
    -not ladder or step or...
   TODO: doesn't even use target-y right now...
    -Should we not create ourselves if the need
     isn't there? Probably..."
  [lev hm target-x target-y]
  (let [x (- target-x 8)
        y (- (get-height hm x) 3)]
    (blockxy! lev x y 4)
    (blockxy! lev (+ x 4) (- y 4) 4)))

(defn overpass!
  "Overpass height right now is 1."
  [lev hm x width]
  (let [height 1]
    (do (steppingstone! lev hm x height)
        (doall (map #(blockxy! lev % height 4) (range x (+ x width)))))))

(defn impediment!
  [lev hm x kind]
  (case kind
        :hill1 nil
        :hill2 nil
        :blocks (blocks! lev hm x 5 0)
        :pipe (pipe! lev hm (+ x 4) 3 false)))

;TODO: the gene structures could probably automatically be tied to the
;      level generation functions based on variable names...
(defn impose-gene [level heightmap {type :type x :x :as gene}]
  (case type
    :enemy (enemy! level heightmap x (:kind gene) ([true false] (rand-int 2)))
    :blocks (blocks! level heightmap x (:width gene) (:coins gene))
    :cannon (cannon! level heightmap x)
    :hole nil #_(hole! level x (:width gene))
    :pipe (pipe! level heightmap x (:height gene) (:piranha gene))
    :height nil
    :enemy-pit (enemy-pit! level heightmap x (:kind gene) (:num gene))
    :enemy-pit-above (enemy-pit-above! level heightmap x (:width gene)
                                       (:kind gene) (:num gene)
                                       (:left gene) (:right gene))
    :enemy-row (enemy-row! level heightmap x (:kind gene) (:num gene)
                           (:shell? gene) (:separation gene))
    :blocks-with-enemies (blocks-with-enemies! level heightmap x (:width gene) (:num gene) (:kind gene) (:coins gene))
    :coin-arc (coin-arc! level heightmap x)
    :coin-row (coin-row! level heightmap x (:width gene))
    :overpass (overpass! level heightmap x (:width gene))
    :hill nil
    :hill-with-enemies (enemy-row-xy! level x (dec (:height gene))
                                      (min (:num gene) (:width gene))
                                      (:kind gene))
    :steps nil
    :impediment (impediment! level heightmap x (:kind gene))
    nil))

(defn generate-level
  ([genotype]
     (generate-level genotype 250))
  ([genotype width]
  (let [level (LevelStub. width 15 0 nil)
        _ (hills! level genotype)
        hm (heightmap genotype)
        _ (doall (for [x (range width) y (range (inc (get-height hm x)) 15)]
                   (.setBlock level x y bground)))        
        _ (doall (map #(impose-gene level hm %) genotype))
        _ (.setxExit level (- width 50)) 
        _ (.setyExit level 13)
        _ (.fixWalls level)]
    level)))

(defn gtest-2
  []
  (let [lev (LevelStub. 250 15 0 nil)
        hm (heightmap [{:type :enemy-pit :x 10 :width 5}
                       {:type :steps :height 5 :width 5 :reverse? false :x 20 :num 2}])
        _ (hills! lev [{:x 10 :w 10 :h 10}
                       {:x 12 :w 4 :h 8}])
        _ (doall (for [x (range 250) y (range (inc (get-height hm x)) 15)]
                   (.setBlock lev x y bground)))
        _ (blocks-with-enemies! lev hm 10 5 3 2)
        _ (.setxExit lev 140) 
        _ (.setyExit lev 13)
        _ (.fixWalls lev)]
    lev))

(defn generate-test-level []
  (let [level (LevelStub. 250 15 0 nil) ;1=underground,2=castle
        _ (doall (for [x (range 250) y (reverse (range 13 15))]
                   (.setBlock level x y bground)))
        _ (.setBlock level 10 12 bground)
        _ (.setBlock level 11 12 bground)
        _ (.setBlock level 13 10 (byte 21))
        _ (.setxExit level 140) 
        _ (.setyExit level 13)
        _ (.fixWalls level)]
    level))




(comment 
(defn test-render [level] (let [bi (new-image 1600 (* 15 16))
        g (.createGraphics bi)
        gc (.getDeviceConfiguration g)
        renderer (LevelRenderer. level gc 1600 120)
        _ (.setCam renderer 0 0)
        _ (.render renderer g 0 0.0)
        ]
    bi)))

;(def lev (generate-level (:genotype (currentbest))))
;(save-image (test-render lev) "firsttrial.png")
;moved to ga.clj
;(ClojureStub/runLevel lev)
