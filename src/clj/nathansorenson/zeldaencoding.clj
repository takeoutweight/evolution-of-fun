(ns nathansorenson.zeldaencoding
  (:refer-clojure)
  (:use [clojure.set])
  (:import  (javax.imageio ImageIO)
            (java.awt.image BufferedImage)
            (java.awt.geom GeneralPath)
            (java.awt BasicStroke Color)
            (java.io File)
            (java.nio ByteBuffer)))

(comment
  
(def *worldx* 50)
(def *worldy* 50)
(def *maxlength* 10)

(def types-dir [:vert :horiz])
(def ztest-hallway {:type :hallway :x 3 :y 3 :length 10 :dir :vert})
(def ztest-room {:type :room :x 0 :y 0 :width 5 :breadth 5})
(def ztest-room2 {:type :room :x 13 :y 13 :width 5 :breadth 5})

(def ztest-genotype (list ztest-room ztest-room2 ztest-hallway))

(defn rand-hallway [] {:type :hallway :x (rand-int *worldx*) :y (rand-int *worldy*)
                    :length (rand-int *maxlength*) :dir (rand-elt types-dir)})

(defn rand-room [] {:type :room :x (rand-int *worldx*) :y (rand-int *worldy*)
                    :width (rand-int *maxlength*) :breadth (rand-int *maxlength*)})

(defn rand-monster [] {:type :monster :x (rand-int *worldx*) :y (rand-int *worldy*)})

(defn rand-zgene []
  ((rand-elt [rand-hallway rand-room rand-monster])))


(def *floorimg* (load-image "../../res/blocks/block.png"))
(def *monsterimg* (load-image "../../res/enemies/goomba2.png"))
(defmulti draw-zgene (fn [bimg gene] (:type gene)))

(defmethod draw-zgene :hallway
  [bimg {:keys [x y length dir]}]
  (case dir
        :vert (doseq [ty (range y (+ y length))]
                (draw-at bimg *floorimg* x ty))
        :horiz (doseq [tx (range x (+ x length))]
                (draw-at bimg *floorimg* tx y)))
  bimg)

(defmethod draw-zgene :room
  [bimg {:keys [x y width breadth]}]
  (doseq [tx (range x (+ x width)) ty (range y (+ y breadth))]
    (draw-at bimg *floorimg* tx ty))
  bimg)

(defmethod draw-zgene :monster
  [bimg {:keys [x y]}]
  (draw-at bimg *monsterimg* x y)
  bimg)

(defn draw-zelda-individual
  [{genotype :genotype}]
  (let [img (new-image 900 900)]
    (doseq [block (filter #(#{:hallway :room} (:type %)) genotype)]
      (draw-zgene img block))
    (doseq [monster (filter #(#{:monster} (:type %)) genotype)]
           (draw-zgene img monster)) ;ensure monsters are drawn on top.
    img))

;(save-image (draw-zgene (new-image 1600 1600) ztest-room) "ztest.png")
;(save-image (draw-zgene (new-image 1600 1600) ztest-hallway) "ztest.png")
;(save-image (draw-zelda-individual {:genotype ztest-genotype}) "ztest.png")

; "returns a list of coordinates for the given gene"
(defmulti zgene-to-coords :type)
(defmethod zgene-to-coords :hallway
  [{:keys [x y length dir]}]
  (case dir
        :vert (for [ty (range y (+ y length))]
                [x ty])
        :horiz (for [tx (range x (+ x length))]
                 [tx y])))
(defmethod zgene-to-coords :room
  [{:keys [x y width breadth]}]
  (for [tx (range x (+ x width)) ty (range y (+ y breadth))]
    [tx ty]))
(defmethod zgene-to-coords :monster
  [_ & rest]
  [])

(defn zgenotype-to-set
  "returns a levelset, a hash-set of coordinates: #{[1 2], [3 5], ...}"
  [genotype]
  (reduce into #{} (map zgene-to-coords genotype)))

(defn zgenotype-to-monsters
  "returns a hash-set of monster coordinates"
  [genotype]
  (into #{} (map #(vector (:x %) (:y %)) (filter #(= :monster (:type %)) genotype))))

(defn neighbours
  [{[x y] :coords} level-set]
  (filter level-set [[(inc x) y]
                     [(dec x) y]
                     [x (inc y)]
                     [x (dec y)]

                     [(inc x) (inc y)] ;diags
                     [(inc x) (dec y)]
                     [(dec x) (inc y)]
                     [(dec x) (dec y)]]))
;(neighbours {:coords [1 1]})

;refactor a* so it's nicer and uses this one, not neighbours above.
;but do we want diagonal a*?
;we use diagonals so we don't bias diagonal movement.
(defn neigbs-TWO [[x y] level-set]
  (filter level-set [[(inc x) y]
                     [(dec x) y]
                     [x (inc y)]
                     [x (dec y)]
                     
                     [(inc x) (inc y)] ;diags
                     [(inc x) (dec y)]
                     [(dec x) (inc y)]
                     [(dec x) (dec y)]]))


;{:coords [0 0] :g 0 :h (distance [0 0] [70 70])}
;TODO: a-star takes weird vectors {:coords [x y]} not just plain coordinates? switch.
(defn a-star [start, goal, level-set]
  (if (or (not (level-set (:coords start)))
          (not (level-set (:coords goal))))
    nil
    (loop [closedmap {}
           openmap {(:coords start) (assoc start 
                                      :g 0 
                                      :h (distance (:coords start) 
                                                   (:coords goal)))}]
      (if (empty? openmap)
        nil                          ;fail TODO: make this more robust
        (let [lowestf (val (first (sort-by #(+ (:g (val %)) (:h (val %))) openmap)))]
          (if (= (:coords lowestf) (:coords goal))
            (:g lowestf)                ;success
            (recur
             (assoc closedmap (:coords lowestf) lowestf)
             (merge (dissoc openmap (:coords lowestf))
                    (into {} (map 
                              #(vector 
                                %
                                {:coords %  :g (inc (:g lowestf)) :h (distance % (:coords goal))})
                              (filter (complement #(closedmap %)) (neighbours lowestf level-set))))))))))))

(defn a-star-2
  "returns a sequence of the path from start to goal. Takes vector coords."
  [start, goal, level-set]
  (if (or (not (level-set start))
          (not (level-set goal)))
    nil
    (loop [closedmap {}
           openmap {start {:g 0
                           :h (distance start, goal)
                           :came-from start}}]
      (if (empty? openmap)
        nil
        (let [lowestf (first (sort-by #(+ (:g (val %)) (:h (val %))) openmap))
              lowestf-coord (key lowestf)
              lowestf-g (:g (val lowestf))]
          (if (= lowestf-coord goal)
            (loop [coord (:came-from (openmap lowestf-coord))   ;reconstruct shortest path
                   path (list lowestf-coord)]  ;maybe simplify this by reworking base case?
              (if (= coord start)
                path
                (recur (:came-from (closedmap coord)) (conj path coord))))
            (recur                       ;open lowestf node and continue
             (assoc closedmap lowestf-coord (val lowestf))
             (into (dissoc openmap lowestf-coord)
                   (map 
                    #(vector
                      %
                      {:g (inc lowestf-g) :h (distance % goal) :came-from lowestf-coord})
                    (filter (complement #(closedmap %))
                            (neigbs-TWO lowestf-coord level-set)))))))))))

(defn max-walk [start level-set]
  (loop [closedset #{}
         openset (intersection #{start} level-set)
         dist 0]
    (if (empty? openset)
      dist
      (recur (union closedset openset)
             (difference 
              (into #{} (mapcat #(neigbs-TWO % level-set) openset)) 
              closedset)
             (inc dist)))))

;(a-star {:coords [0 0]} {:coords [0 2]} #{[0 0] [41 1] [0 2]})

;; (zfeasible? [{:type :hallway :x 5 :y 5 :dir :vert :length 50}
;;             {:type :hallway :x 5 :y 50 :dir :horiz :length 50}])

;; (save-image 
;;  (draw-zelda-individual {:genotype [{:type :hallway :x 5 :y 5 :dir :vert :length 50}
;;                         {:type :hallway :x 5 :y 50 :dir :horiz :length 50}]})
;;  "test.png")

;; (zgenotype-to-set [{:type :hallway :x 5 :y 5 :dir :vert :length 50}
;;                   {:type :hallway :x 5 :y 50 :dir :horiz :length 50}])

;;(def tindiv {:genotype [{:type :hallway :x 5 :y 5 :dir :vert :length 50}
;;                         {:type :hallway :x 5 :y 50 :dir :horiz :length 50}]})


;(count (filter (complement (zgenotype-to-set genotype))
;                   (for [x (range 5 50)] [x x])))

;(zfitness-constraint {:genotype [{:type :room :x 30 :y 30 :width 1 :breadth 5}]})

;CAUTION: THIS INTENTIONALLY CLOBBERS STUFF FROM flex! 

(defn rand-gene []
  ((rand-elt [rand-room rand-hallway rand-monster])))

;isn't totally polymorphic yet. Doesn't stretch hallway lengths.
(defn mutate-gene [{:keys [x y] :as zgene}]
  (case (rand-elt [0 0 0 0 1 1 2])
        0 (-> zgene (assoc :x (clamp (int (+ x (rand-gauss 4))) 0 55))
                    (assoc :y (clamp (int (+ y (rand-gauss 4))) 0 55)))
        1 (rand-gene)
        2 nil))

(def *injected-genes*
  [
   ;{:type :hallway :x 20 :y 10 :length 20 :dir :vert}
   ;{:type :hallway :x 22 :y 10 :length 20 :dir :vert}
   ;{:type :hallway :x 24 :y 10 :length 20 :dir :vert}
   ;{:type :hallway :x 26 :y 10 :length 20 :dir :vert}
   
   ;{:type :hallway :x 20 :y 29 :length 2 :dir :horiz}
   ;{:type :hallway :x 24 :y 10 :length 2 :dir :horiz}
   ])


(defn flood-expand
  "expands a coord-set from a given point for a distance.
   TODO: Maybe shoulZd combine with max-walk somehow."
  [start-coord level-set distance]
  (loop [closedset #{}
         openset #{start-coord}
         dist distance]
    (if (or (empty? openset) (zero? dist))
      closedset
      (recur (union closedset openset)
             (difference 
              (into #{} (mapcat #(neigbs-TWO % level-set) openset)) 
              closedset)
             (dec dist)))))

(defn manh-dist [[x y] [a b]]
  (+ (Math/abs (- x a))
     (Math/abs (- y b))))

(defn walk-level
  "get the challenge sequence of a level."
  [start-coord end-coord level-set monster-set]
  (loop [closedset #{}
         openset (intersection #{start-coord} level-set)
         location start-coord
         monst monster-set
         challenge []]
    (if (or (not (empty? (intersection openset #{end-coord})))
              (empty? openset))
        challenge
        (let [visibleset (flood-expand location level-set 4)
          closableset (flood-expand location level-set 3)
          new-closedset (union closedset closableset)
          new-openset (difference (union openset visibleset)
                                   new-closedset)
          nearby-monsters (intersection monst visibleset)       
          dist-start #(manh-dist % start-coord)
          dist-loc #(manh-dist % location)
          dist-weighted (fn [coord] (- (dist-start coord)
                                       (/ (dist-loc coord) 1000))) ;favour nearby goals
          goal (last (sort-by dist-weighted new-openset))
          next-loc (first (sort-by #(a-star {:coords %} {:coords goal} level-set)
                                     (neigbs-TWO location level-set)))]
          (recur new-closedset
               new-openset
               next-loc
               (difference monst nearby-monsters)
               (conj challenge (count nearby-monsters)))))))

(defn walk-level2
  "simplified exploration model, just finds shortest path and walks it."
  [start-coord end-coord level-set monster-set]
  (loop [chal-seq []
         path (a-star-2 start-coord end-coord level-set)
         m-set monster-set]
    (if (empty? path)
      chal-seq
      (let [near-monsters (intersection
                           (flood-expand (first path) level-set 4)
                           m-set)]
        (recur (conj chal-seq (count near-monsters))
               (rest path)
               (difference m-set near-monsters))))))

(defn zrhythm
  "the rhythm group model, dealing with enemy list"
  [{:keys [fun mode anxiety]} enemies]
  (let [impact (if (= 0 enemies)
                 -0.5
                 enemies)]
    {:anxiety (+ anxiety impact)
     :mode (if (> anxiety 5)
             -1
             (if (< anxiety 0)
               1
               mode))
     :fun (+ fun (* mode impact))}))

(defn zfitness-optimize 
 [{genotype :genotype}]
 (+ 
  (* 0.1 (count genotype))
  (if (> (count genotype) 1000)
    (count genotype)
    0)
  (let [challenge-vec (walk-level2 [5 50] [50 5]
                                   (zgenotype-to-set genotype)
                                   (zgenotype-to-monsters genotype))]
    (* -1 (:fun (reduce zrhythm {:anxiety 0 :mode 1 :fun 0} challenge-vec))))))


(defn zfeasible? [{genotype :genotype}]
  (not (nil? (a-star {:coords [5 50]} {:coords [50 5]} (zgenotype-to-set genotype)))))

(defn zfitness-constraint
  "count how far one can walk from each end.
   we need some negative pressure on genotype size though, otherwise we waste time with MASSIVE genotypes."
  [{genotype :genotype}]
  (+ (if (> (count genotype) 100) (- (count genotype) 100) 0)
     (- (max-walk [5 50] (zgenotype-to-set genotype)))
     (- (max-walk [50 5] (zgenotype-to-set genotype)))))

;(def testindiv {:genotype (concat (:genotype (currentbest)) [{:type :monster :x 5 :y 50}])})
;(def testset (zgenotype-to-set (:genotype (currentbest))))

;; (def dist-start #(manh-dist % [0 0]))
;; (def dist-loc #(manh-dist % [2 0]))
;; (def dist-weighted (fn [coord] (- (dist-start coord)
;;                                       (/ (dist-loc coord) 1000))))

;; (reduce zrhythm {:anxiety 0 :mode 1 :fun 0}
;;        (walk-level2 [5 5] [50 50]
;;                     (zgenotype-to-set (:genotype (currentbest)))
;;                     (zgenotype-to-monsters (:genotype (currentbest)))))

;; (walk-level
;;  [0 0]
;;  [6 3]
;;  #{[0 0] [1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [6 1] [6 2] [6 3]}
;;  #{[4 0] [6 3] [7 3]})

;; (def my-level #{[0 0] [0 1] [41 1] [0 2]})
;; (a-star {:coords [0 0]} {:coords [0 2]} #{[0 0] [0 1] [41 1] [0 2]}

;; (first (sort-by #(a-star {:coords %} {:coords [10 0]} my-level)
;;                                     (neigbs-TWO [0 0] my-level)))
) ;comment
