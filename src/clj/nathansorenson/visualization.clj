
;; visualization

(ns nathansorenson.visualization
	(:refer-clojure)
	(:use [nathansorenson flexencoding leveleval imageutils])
	(:import 	(javax.imageio ImageIO)
						(java.awt.image BufferedImage)
						(java.awt.geom GeneralPath)
						(java.awt BasicStroke Color)
						(java.io File)
						(java.nio ByteBuffer)
						))

(def *blockimg* (load-image "res/blocks/block2.png"))
(def *blocksize* 16)

(defn draw-at
 "in object coords. TODO: don't hardcode offsets" 
 [bimg img x y]
	(draw-img bimg img
						(int (* (- x 0) *blocksize*))	;convert to image coordinates
						(int (- (.getHeight bimg) (* (+ y 1) *blocksize*)))))

(defn level-image
	"returns a bufferedImage of a rendered level."
	[level]
	(let [[minx miny maxx maxy] (bounds level)
				width  (+ (* *blocksize* (- maxx minx)) *blocksize*)
				height (+ (* *blocksize* (- maxy miny)) *blocksize*)
				img (new-image  width height)]
		(doseq [b level] (draw-at img *blockimg* (first b) (second b)))
		img))


;;;---------- draw-gene ----------

(defmulti draw-gene (fn [bimg gene & rst] (:type gene)))
(defmethod draw-gene :default
	[bimg & rest]
	bimg)

(defmethod draw-gene :block
	[bimg {:keys [x y]}]
	(draw-at bimg *blockimg* x y))

(def *stairimg* (load-image "res/blocks/block3.png"))
(defmethod draw-gene :stairs
	[bimg gene]
	(let [blocks (gene-to-blocks gene)]
		(doseq [[x height] blocks, y (range height)]
			(draw-at bimg *stairimg* x (inc y)))
		bimg))

(def *pipetop* (load-image "res/blocks/pipetop.png"))
(def *pipebottom* (load-image "res/blocks/pipebottom.png"))
(defmethod draw-gene :pipe
	[bimg {:keys [x height]}]
	(draw-at bimg *pipetop* x height)
	(doseq [y (rest (range height))]
		(draw-at bimg *pipebottom* x y))
	bimg)
	

(def *platl* (load-image "res/blocks/greenl.png"))
(def *platr* (load-image "res/blocks/greenr.png"))
(def *platm* (load-image "res/blocks/greenm.png"))
(def *platb* (load-image "res/blocks/greenbottom.png"))
(defmethod draw-gene :plat-raised
	[bimg gene]
	(let [blocks (gene-to-blocks gene)]
		(draw-at bimg *platl* (first (first blocks)) (second (first blocks)))
		(doseq [[x y] (drop-last (rest blocks))]
			(draw-at bimg *platm* x y))
		(draw-at bimg *platr* (first (last blocks)) (second (last blocks)))
		(doseq [[x height] (rest (drop-last blocks)), y (range height)]
			(draw-at bimg *platb* x y))
		bimg))

;(draw-gene (new-image 1600 320) (rand-stairs))
;(def rs (rand-stairs))
;(def rp (rand-plat-raised))
;(save-image (draw-gene (new-image 1600 320) rp) "test.png")
;(save-image (draw-gene (new-image 1600 320) rs) "test.png")
;(save-image (draw-gene (new-image 1600 320) (rand-pipe)) "test.png")
;(save-image (draw-at (new-image 1600 1600) *stairimg* 99 99) "test.png")

(def *enemyimg* (load-image "res/enemies/goombal.png"))

(defn draw-gene-enemy
	"requires a special appraoch, as they sit atop a reified level"
	[bimg {x :x} block-map]
	[bimg {:keys [x]}]
	(let [height (get block-map x)
				y (if height
						(inc height)
						1)]
		(draw-at bimg *enemyimg* x y)))

(def *groundimg* (load-image "res/blocks/block.png"))
(defn draw-ground
	"draws the underlying ground, with the holes missing as proper.
   given a map of x=>y values,"
	[bimg block-map]
	(doseq [x (keys block-map)]
		(draw-at bimg *groundimg* x 0))
	bimg)

;(save-image (draw-ground (new-image 1600 320) {1 0 2 0 3 0}) "test.png")


(defn draw-individual
	"draws an individual"
	[{genotype :genotype}]
	(let [img (new-image 1600 320)
				blockmap (flex-to-block-map genotype)]
		(doseq [gene genotype] (draw-gene img gene))
		(doseq [gene (filter #(= (:type %) :enemy) genotype)] 
			(draw-gene-enemy img gene blockmap))
		(draw-ground img (fill-ground {} (reify-holes genotype)))
		img))

;(fill-ground {} (reify-holes genotype-to-blocks))
;(save-image (indiv-image (rand-individual)) "test.png")
;(save-image (indiv-image {:genotype [{:type :hole :x 7 :width 24}]}) "test.png")
;(reify-holes [{:type :hole :x 7 :width 24}])

(defn draw-statehistory
	[bi statehistory key]
	(let [lines (into [] (map vector (range (count statehistory)) 
	                                 (map key statehistory)))
	      tlines (scale-to-image lines bi)
	      labels (map conj (vec (take-nth 10 tlines)),
	                       (map second (take-nth 10 lines)))
	       ]
		(draw-line bi tlines)
		(draw-labels bi labels)))

(defn to-csv
	"Converts a statehistory into a comma seperated value file."
	[statehistory filename]
	(loop [[head & rest] statehistory, st ""]
		(if (nil? head)
			(spit filename st)
			(recur rest, 
						(reduce #(.concat %1 %2) 
												[st, (str (:anxiety head)), ",", (str (:isrelaxing head)), "\n"])))))

(defn visualize
	"outputs a png image visualization of the player model"
	[level filename]
	(save-image (draw-statehistory (level-image level) (level-eval level) :anxiety) filename))
;(visualize lev "lev.png")

(comment
(save-image 
	(let [bi (new-image 500 100)]
	(draw-statehistory bi
											(leveleval (delta-to-abs (:genotype (currentbest))) [*initial-state*]) 
											:anxiety))
"anxiety.png")
)

;(save-image (level-image (delta-to-abs (:genotype (currentbest)))) "image.png")
;(save-image (level-image (delta-to-abs [[3 4][64 4][10 10][128 64]])) "image.png")
;(save-image (level-image [[0 4][64 4][128 64]]) "image.png")

(comment
;caution, it seems as if smclvl can't jump quite as high.
(defn pixel? [bi x y] (> (first (getRGB bi [x y])) 0.5))
(def a (load-image "mario/1-3.png"))
(def ma (into [] (for [x (range 165) y (range 14)] [x y (pixel? a x y)])))
;map to 64x48 (lower height because smc can't jump so much.
(def mal (vec (map #(vector (* 64 (first %)) (* 48 (second %)))
				   (filter #(= (third %) true) ma))))
(to-csv (leveleval mal) "1-3.csv")
;(save-level mal "2-3.smclvl")
;(visualize mal "2-3.png")
(save-image 
	(let [bi (new-image 500 100)]
	(draw-statehistory bi
											(leveleval mal) 
											:anxiety))
"anxiety.png")
);comment


;eof
