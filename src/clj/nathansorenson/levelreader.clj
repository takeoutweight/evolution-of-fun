
;; levelreader

(ns nathansorenson.levelreader
	(:use [nathansorenson imageutils nutils]
				[clojure.set :as set]
				[incanter core stats])
	(:import 	(le Optimized)
						#_(name.audet.samuel.javacv.jna cv highgui cxcore
																					cxcore$CvMat cxcore$CvPoint cxcore$CvScalar
																					cxcore$CvPoint$ByValue cxcore$CvScalar$ByValue)
						#_(com.sun.jna.ptr DoubleByReference)
						(java.io FileWriter File PushbackReader FileReader)))

(defn save-dat [file-name obj] 
  (with-open [w (FileWriter. (File. file-name))] 
    (binding [*out* w *print-dup* true] (prn obj)))) 

(defn load-dat 
  "Load a clojure form from file" 
  [file] 
  (with-open [r (PushbackReader. (FileReader. file))] 
    (let [rec (read r)] 
      rec)))

(defn ls
	"gets filenames of all files in directory.
   FIXME: currently returns foldernames too. (not just files)"
	[folder]
	(let [filenames (map #(.getName %) (.listFiles (java.io.File. folder)))
				f2 (map #(.concat "/" %) filenames) 
				complete-fnames (map #(.concat folder %) f2)]
		complete-fnames))

(defn load-folder
	"loads all files in a folder into a seq of datastructors."
	[folder]
	(map #(load-dat %) (ls folder)))

(defn template-match
	"returns a vec of [x,y] pairs of the template in the image.
	 Auto-loads files if string pathnames are given instead of
	 BufferedImages."
	[template image]
	(let [maybe-load (fn [resource] 	
									 	(if (string? resource) (load-image resource) resource))]
		(Optimized/filterMap (maybe-load template) (maybe-load image))))

(defn folder-match
	"matches a whole folder of templates against a level. Unions the results"
	[prefix folder level]
	(save-dat (str "res/out/" prefix "-" (re-find #"[\w-]+\." level) "txt")
						(reduce set/union (pmap #(template-match % level) (ls folder)))))				

(defn folder-folder-match
	"matches an entire folder against an entire folder.
   TODO: could be refactored with folder-match.
   returns nothing.
   but saves to file along the way."
		[prefix template-folder level-folder]
		(doall (map #(folder-match prefix template-folder %) (ls level-folder))))

;(folder-folder-match "blocks" "res/blocks" "res/levels")

(defn connect-4
	[blockset selectedset]
	(let [adjacent (fn [[x y]]
										#{[(inc x) y]
											[(dec x) y]
											[x (inc y)]
											[x (dec y)]})
				neighbours (fn [set]
										 (apply union (map adjacent set)))]
		(loop [closed #{}
					 open selectedset]
			(if (empty? open)
				closed
				(recur (union closed open)
							 (difference (intersection (neighbours open)
																				 blockset)
													 closed))))))

(defn flatten-level
	"projects level to horiz. axis. To find holes. Also norms by dividing by 16."
	[blockset]
	(vec (into (sorted-set)
						 (map #(int (/ % 16)) (map first blockset)))))

(defn level-holes
	[blockset]
	(let [flat (flatten-level blockset)
				deltas (map #(- %2 %1 1) flat (rest flat))]
		(filter #(not (= 0 (second %)))
						(map vector flat deltas))))

(defn dat-to-genotype
	"Takes template data and constructs a level genotype from it"
	[blockset enemyset]
	(let [holes (map #(hash-map
										 :type :hole
										 :x (first %)
										 :width (second %)) (level-holes blockset))
				enemies (map #(hash-map
											 :type :enemy
											 :x %
											 :kind 0) (flatten-level enemyset))
				length (int (/ (last (sort (map first blockset))) 16))]
		(vec (sort-by :x (concat holes
														 enemies
														 [{:type :sentinel
															 :x length
															 :difficulty 0}])))))

(defn dat-to-indiv
	[blockset enemyset]
	{:genotype (dat-to-genotype blockset enemyset)})

(defn old-level-holes
	"FIXME:
   this only works if the holes don't have overhangs or floating platforms
   because it basically only measures the lowest level of blocks...
   MAYBE: Try to see if a maybe-hole has a block above it... if so, does *that* block
   have a  non-block above *it* (i.e. not a roof-block) if so, count that a
   non-hole segment...

   I could remove the ceiling, by selecting the highest row + all 4 connected.
   EXCEPT the very left-most column, as that sometimes 4-connects to the floor.
   i.e. in 4-2 and 1-2."
	[blockset]
	(let [lowest (last (sort  (map second blockset)))
				groundlevel (sort (map first (filter #(= lowest (second %)) blockset)))
				normed (map #(/ % 16)
										(if (not (= 0 (first groundlevel)))
											(map #(- % (first groundlevel)) groundlevel)
											groundlevel))
				deltas (map #(- %2 %1 1) normed (rest normed))]
		(filter #(not (= 0 (second %)))
						(map vector normed deltas))))

;1,3,6,10,14,18,22 (12 is missed It was before I lowered the ceiling anyway.)
;the number of ceilings range from 224-318. The next highest is 67 on level "26"
;a safe threshold would then be 145. 
(defn count-ceiling
	[blockset]
	(let [highest (first (sort (map second blockset)))]
		(count (filter #(= highest (second %)) blockset))))

(defn remove-ceiling [blockset]
	"FIXME: this doesn't seem to be working, but I just pre-photoshopped the
   levels ahead of time.
   returns a blockset without its 4-connected ceiling.
   I guess the number of roof-blocks necessary for a ceiling to exist is
   around 145."
	(let [highest (first (sort (map second blockset)))
				toprow (filter #(= highest (second %)) blockset)]
		(if (> (count toprow) 145)
			(let [ceiling (connect-4 blockset
															 (filter #(= highest (second %))
																			 blockset))]
				(difference blockset ceiling))
			blockset)))


(defn scramble-level
	[genes]
	(let [max-x (:x (last genes))]
		(conj
		 (vec (sort-by :x (map #(assoc % :x (rand-int max-x))
													 (take (dec (count genes)) genes))))
		 (last genes))))

(defn gaussian-filter
	[t std-dev xs]
	(reduce + (map #(pdf-normal (- t %) :sd std-dev) xs)))

;view a smoothed (convolved) version of enemy density.
;(view (function-plot #(gaussian-filter % (first xs)) 0 230))

;it's in nutils now.
#_(defn separation
	"finds the distances between every consecutive enemy."
	[xs]
	(map #(- %2 %1) xs (rest xs)))

(defn count-max-mins
	"counts the number of local maxima & minima"
	[xs]
	(let [sep (separation xs)]
		(second (reduce (fn [[pos-deriv?, count] next]
							(if (= pos-deriv? (pos? next))
								[pos-deriv?, count]
								[(not pos-deriv?), (inc count)]))
						[(pos? (first sep)), 0]
						sep))))

;to get a huge list of every separation value.
;(def sep (apply concat (map #(separation (map :x %)) *levels*)))
;(def negsep (apply concat (map #(separation (map :x %)) *negative-levels*)))

;to get the separation values per level.
;(def level-sep (map separation (map #(map :x %) *levels*)))

;enemy x coords
;(def level-xs (map #(map :x %) *levels*))
;(def nevel-xs (map #(map :x %) *negative-levels*))

;gaussian filtered enemy densities
;(def level-ga (map (fn [lev] (map #(gaussian-filter % 4 lev) (range 250))) level-xs))
;(def nevel-ga (map (fn [lev] (map #(gaussian-filter % 4 lev) (range 250))) nevel-xs))

(defn smooth
	"gives a smoothed timeseries given a list of impulse x points"
	[impulse-xs filter-width length]
	(map #(gaussian-filter % filter-width impulse-xs) (range length)))

#_(def xy2 (dynamic-xy-plot
				 [n2 (range (count *levels*))]
				 [(range (count (nth nevel-ga (int n2))))
					(nth nevel-ga (int n2))]))

#_(doall (for [ga (rest nevel-ga)]
				 (add-lines xy (range (count ga)) ga)))
#_(view xy2)

;shows a scatterplot
#_(doto
		(scatter-plot (range (count *levels*))
									(map (fn [ga]
												 (count-max-mins ga))
											 level-ga))
	(add-points (range (count *levels*))
							(map (fn [ga]
												 (count-max-mins ga))
											 nevel-ga))
	(view))


;This is the number of 0-steps between enemies. A very salient feature!
#_(map (fn [lev] (count (filter #(< % 0.5) lev))) level-sep)

;This is the maximum enemy density after a filtering smooth.
#_(map (fn [lev]
				 (apply max (map (fn [t] (gaussian-filter t lev))
												 (range 250))))
			 (map #(map :x %) *levels*))

;how to get a "mode" or most common value + gaussian filtering.
;a maximum of he histogram. (I think.)
#_(map #(apply max %)
			 (map (fn [lev] (map #(gaussian-filter % lev) (range 20)))
						level-sep))

;shows an overlapping chart, horizontally log-scaled.
#_(doto
	(function-plot
	 #(gaussian-filter % (map (comp log inc) sep)) 0 5)
	(add-function
	 #(gaussian-filter % (map (comp log inc) negsep)) 0 5)
	(view))

;same thing, vertically log-scaled
#_(doto
	(function-plot
	 #((comp log inc gaussian-filter) % sep) 0 50)
	(add-function
	 #((comp log inc gaussian-filter) % negsep) 0 50)
	(view))

;same thing, vertically & horizontally log-scaled
#_(doto
	(function-plot
	 #((comp log inc gaussian-filter) % (map (comp log inc) sep)) 0 5)
	(add-function
	 #((comp log inc gaussian-filter) % (map (comp log inc) negsep)) 0 5)
	(view))

(comment             ;------------SCRATCH----------------

(str (fourth (.listFiles (java.io.File. "."))))
(count (template-match "res/blocks/block3.png" "res/levels/1-1full.png"))
(def t (map #(.getName %) (.listFiles (java.io.File. "res/blocks"))))
(def t2 (map #(.concat "/" %) t))
(map #(.concat "res/blocks" %) t2)

(def en (folder-match "res/enemies" "res/levels/1-1full.png"))

(def im (load-image "res/enemies/koopa.png"))
(Optimized/getRGB im 4 0)

) ;comment              -----------SCRATCH--------------

;(def tm2 (folder-match "res/enemies" "res/levels/mario-1-1.png"))
;(time (def tm2 (template-match "res/enemies/bulletbill2.png" "res/levels/mario-5-1.gif")))
;(def im1 (load-image "res/levels/mario-5-1.gif"))
;(time (Optimized/fastImgFilter (load-image "res/enemies/bulletbill2.png") (load-image "res/levels/mario-1-2.png") 0 0))
;(time (Optimized/fastImgFilter (Optimized/convertToCheckable (load-image "res/enemies/bulletbill2.png")) (Optimized/convertToCheckable (load-image "res/levels/mario-1-2.png")) 0 0))
;(def cv3 (highgui/cvLoadImage "res/levels/mario-1-2.png" 1))

;we get high values in an area... (I wonder if the alpha mask isn't working right...)
;(def cv1 (highgui/cvLoadImage "res/blocks/block6.png" 1))
;(def cv2 (highgui/cvLoadImage "res/levels/mario-1-2.png" 1));

;(def cvout (cxcore$CvMat/create 209 3041 cxcore/CV_32F))

;(time (cv/cvMatchTemplate cv2 cv1 cvout 0))
;(cxcore/cvConvertScale cvout cvout 0.0000250 0)

;(def minval (DoubleByReference.))
;(def minpt (cxcore$CvPoint.))
;(def point (cxcore$CvPoint$ByValue. 193 221))
;(def point (cxcore$CvPoint$ByValue. 221 193)) ;not sure of what coordinate is x
;(def scalar (cxcore$CvScalar$ByValue. 200.5 10.5 10.5 10.5))
;(cxcore/cvMinMaxLoc cvout minval nil minpt nil nil)
;(.put cvout 194 221 100.0)
;(cxcore/cvCircle cvout point 10 scalar -1 8 0)
;(cv/cvSmooth cv1 cv1 2 3 0 0 0)
;(highgui/cvSaveImage "res/out/test.png" cvout)
;3056x224 and 15x16
