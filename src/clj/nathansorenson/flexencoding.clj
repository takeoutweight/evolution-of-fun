;; flexencoding.clj 
(ns nathansorenson.flexencoding
	(:refer-clojure)
	(:use [clojure.set]
				[nathansorenson.nutils])
	(:import (JaCoP.core Store GoalVar)
					 (JaCoP.constraints XneqY)
					 (JaCoP.constraints.geost Geost GeostObject DBox Shape NonOverlapping)
					 (JaCoP.search DepthFirstSearch IndomainGoal InputOrderSelectByVar)))

(def initial-dmz 10)
(def level-length (atom 130)) ;211 mario 1-1 length.
(def *max-height* 3) ; + 1 cuz no zero heights..maximum reasonable height
;(def *max-hole-width* 3)
(def *max-hole-width* 4)
(def *max-plat-width* 10) ;+3 cuz they can't be less than 3 tiles.

(def types-kind [:goomba, :koopa])
(def types-directions [:ascend :descend])
(def types [:block :enemy :stairs :pipe :hole :plat-raised])

(def test-enemy 	 {:type :enemy, 	:x 0, :kind :goomba})
(def test-block 	 {:type :block, 	:x 0, :y, 1})
(def test-stairs 	 {:type :stairs, 	:x 1, :height 3, :direction :descend})
(def test-pipe 		 {:type :pipe, 		:x 5, :height 5, :piranha true})
(def test-hole 		 {:type :hole, 		:x 6, :width 20})
(def test-hole2		 {:type :hole, 		:x 30 :width 3})

(def test-plat-raised {:type :plat-raised, :x 8, :height 4, :width 5})
;TODO if I have time...
;(def test-plat-moving {:type :plat-moving, :x, 14, :height 0 :width 3, :range 4})

(def test-genotype (list
										  test-block, test-enemy, test-stairs, test-pipe, 
											test-hole, test-hole2, test-plat-raised))

(defn rand-bool [] ([true false] (rand-int 2)))
(defn rand-x [] (+ initial-dmz (rand-int (- @level-length initial-dmz 60))))
(defn rand-y [] (rand-int *max-height*))
(defn rand-kind [] (rand-elt types-kind))
(defn rand-height [] (inc (rand-int *max-height*)))
(defn rand-direction [] (rand-elt types-directions))

(defn rand-block [] {:type :block, :x (rand-x) :y (rand-y)})
(defn rand-stairs [] {:type :stairs, :x (rand-x) 
									 :height (rand-height) 
									 :direction (rand-direction)})
(defn rand-pipe [] {:type :pipe :x (rand-x) :height (+ 1 (rand-height)) 
										:piranha (rand-bool)})
(defn rand-hole [] {:type :hole :x (rand-x) :width (inc (rand-int *max-hole-width*))})
(defn rand-plat-raised [] {:type :plat-raised 
													 :x (rand-x) 
													 :width (+ 3 (rand-int *max-plat-width*))
													 :height (rand-height)})
(defn rand-blocks [] {:type :blocks, :x (rand-x) :width (inc (rand-int *max-hole-width*))
											:coins (rand-int 2)})
(defn rand-cannon [] {:type :cannon :x (rand-x) :height (rand-height)})
(defn rand-enemy [] {:type :enemy, :x (rand-x) :kind (rand-int 4) :num 1})
(defn rand-step [] {:type :step, :x (rand-x) :height (rand-int 4)}) ;??? What's this for? don't we have stairs already?
;these are the new ones:
(defn rand-enemy-pit [] {:type :enemy-pit
												 :x (rand-x)
												 :width (+ 4 (rand-int 4))
												 :kind (rand-int 4)
												 :num (+ 1 (rand-int 3))
												 :shell? (rand-bool)})
(defn rand-enemy-pit-above [] {:type :enemy-pit-above
															 :x (rand-x)
															 :width (+ 7 (rand-int 7))
															 :kind (rand-int 4)
															 :num (+ 1 (rand-int 3))
															 :left (rand-elt [:pipe :cannon :rock])
															 :right (rand-elt [:pipe :cannon :rock])})
(defn rand-blocks-with-enemies [] {:type :blocks-with-enemies
																	 :x (rand-x)
																	 :width (+ 3 (rand-int 4))
																	 :coins (rand-int 4)
																	 :kind (rand-int 3) ;no spikeys
																	 :num (inc (rand-int 4))})
(defn rand-enemy-row [] {:type :enemy-row
												 :x (rand-x)
												 :kind (rand-int 3) ;not spikes
												 :num (+ 2 (rand-int 3))
												 :separation (inc (rand-int 3))
												 :shell? (rand-bool)})
(defn rand-coin-arc [] {:type :coin-arc
												:x (rand-x)})
(defn rand-coin-row [] {:type :coin-row
												:x (rand-x) :width (inc (rand-int 6))})
(defn rand-overpass [] {:type :overpass
												:x (rand-x)
												:width (+ 10 (rand-int 10))})
(defn rand-hill [] {:type :hill
										:x (rand-x)
										:height (- 11 (rand-int 5))
										:width (+ 3 (rand-int 10))})
(defn rand-hill-with-enemies [] {:type :hill-with-enemies
																 :x (rand-x)
																 :height (- 11 (rand-int 5))
																 :width (+ 3 (rand-int 10))
																 :kind (rand-int 4)
																 :num (inc (rand-int 4))})
(defn rand-steps [] {:type :steps
										 :x (rand-x)
										 :height (inc (rand-int 4)) ;TODO steps use bottom-up height, make this consistent
										 :width (+ 3 (rand-int 13))
										 :reverse? (rand-bool)
										 :num (+ 1 (rand-int 4))})
(defn rand-impediment [] {:type :impediment
													:x (rand-x)
													:width 12
													:kind (rand-elt [:hill1 :hill2 :blocks :pipe])})

(defn rand-gene
	"creates a random gene by calling random gene constructor"
	[]
	(let [constructors [rand-enemy, rand-blocks, rand-pipe
											rand-hole, rand-hole, rand-hole, rand-hole,
											rand-hole,
											rand-cannon, rand-enemy-pit rand-enemy-pit-above
											rand-enemy-row rand-blocks-with-enemies
											rand-coin-arc rand-coin-row #_rand-overpass
											rand-hill rand-hill
											rand-hill-with-enemies rand-steps
											rand-impediment]]
		((rand-elt constructors))))

(defn mutate-gene
	"TODO: make this actually mutate different variable types properly."
	[gene] 
	(if (< (rand-int 100) 85)
		(rand-gene) 
		nil))


(defmulti gene-to-blocks
	"Converts a particular gene to a list of block absolute [x,y] representations.
	 empty list if un-convertible type."
	:type)
(defmethod gene-to-blocks :default [_]
	(list))
(defmethod gene-to-blocks :block [b]
	(list [(:x b) (:y b)]))
(defmethod gene-to-blocks :stairs [s]
	(let [[init-height step] (case (:direction s)
													:ascend [1 inc]
													:descend [(:height s) dec])]
		(take (:height s) 
					(map vector 
							 (iterate inc (:x s)) 
							 (iterate step init-height)))))
(defmethod gene-to-blocks :pipe [p]
	(list [(:x p) (:height p)] 
				[(inc (:x p)) (:height p)]))
(defmethod gene-to-blocks :plat-raised [p]
	(take (:width p) (map vector 
													(iterate inc (:x p))
													(repeat (:height p)))))

;; width functions, useful for overlap checks

(defn get-xs [{:keys [x] :as gene}]
	(let [single (fn [] [x])
		    double (fn [] [x (inc x)])
				wide (fn [] (range x (+ x (:width gene))))]
			(case (:type gene)
				:enemy (single)
				:pipe [(dec x) x (inc x) (+ 2 x)]
				:block (single)
				:stairs (range x (+ x (:height gene)))
				:plat-raised (wide)
				:blocks (wide)
				:hole (concat [(- x 2) (dec x)] (wide))
				:cannon [(dec x) x (inc x)]
				:enemy-pit (concat [(- x 2) (dec x)] (wide))
				:enemy-pit-above (concat [(- x 2) (dec x)] (wide))
				:enemy-row (range x (inc (+ x (* (:num gene) (:separation gene)))))
				:blocks-with-enemies (wide)
				:coin-arc (range x (+ x 4))
				:coin-row (wide)
				:overpass (wide)
				:hill (wide)
				:hill-with-enemies (wide)
				:steps (wide)
				:impediment (wide))))

(defn reify-holes
	"creates a hash-set of all hole positions from a genotype eg: #{1 2 4 5 6}"
	[genotype]
	(let [expand-hole (fn [gene] (take (:width gene) (iterate inc (:x gene))))]
	 (into #{} (mapcat expand-hole (filter #(= (:type %) :hole) genotype)))))

(defn genotype-to-blocks
	 "converts a genotype level to a sorted map of x=>y abs coords of blocks,
		sorted by x position. Just the topmost blocks for running upon,
		not all the blocks which make up the visual appearance"
	[genotype]
	(into (sorted-map) (mapcat gene-to-blocks genotype)))

(defn fill-ground
	"Fills in empty spots where there ought to be ground, given a sorted hash of
	 blocks x=>y and a hash-set of hole x coords. Returns filled sorted-map,
	 if block-map is sorted."
	[block-map hole-set]
	(let [ground-map (into (sorted-map) (map vector
																 (difference (into #{} (range @level-length)) hole-set)
																 (repeat 0)))]
		(merge ground-map block-map)))

;(fill-ground (genotype-to-blocks test-genotype) (reify-holes test-genotype))
(defn flex-to-block-map
	"returns x=>y reified level"
	[flex-genotype]
	(fill-ground (genotype-to-blocks flex-genotype) (reify-holes flex-genotype)))

(defn flex-to-block-level
 "Takes a flex encoded genotype and converts it into a block-level, which is a
	sorted vector of [x,y] vectors."
 [flex-genotype]
 (vec (flex-to-block-map flex-genotype)))


;single x=[0,1) 2d "Shape".
;indexed by first paraemeter (int), 
(defn make-shape [width] (Shape. width [(DBox. (int-array [0]) (int-array [width]))]))
;(Shape. 0 [(DBox. (int-array [0]) (int-array [1]))])
;dbox is origin, extent, using arrays as coordinates
;50 right now is maximum shape width.
(def shapes (map make-shape (range 0 50)))

(defn gene-to-width [gene]
	(count (get-xs gene)))

(defn solve-store!
	"mutably assigns labels to varray. Returns boolean true if successful"
	[store varray]
	(.labeling (DepthFirstSearch.) 
						 store
						 (InputOrderSelectByVar. store varray (IndomainGoal.))))



;(repair-genotype [{:hey :jude} {:type :joe} {:type :joe}])
;(def tg1 {:type :pipe :x 3})
;(def tg3 {:type :pipe :x 4 :height 3 :piranha true})
;(def tg4 {:type :stairs :x 4 :height 3})
;(repair-genotype [tg3 tg4])
;(def tg2 {:type :stairs :x 1 :height 4})
;(repair-genotype [tg1 tg2])
;(gene-to-width tg1)