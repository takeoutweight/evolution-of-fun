(ns nathansorenson.gamma-model
  "Gamma Model was the code-name for the final rhythm-group model used
   in the papers. Model was fit with our GA. Why not."
	(:refer-clojure :exclude [pop])
	(:require [nathansorenson.ga :as ga]
						[incanter.distributions :as id])
	(:use [nathansorenson nutils levelreader]
				[incanter core stats charts datasets pdf])
	(:import ;[flanagan.math FourierTransform] ; Lib no longer public: http://www.ee.ucl.ac.uk/~mflanaga/java/index.html
					 [java.awt Font]))

(def params
		 (atom {:size 60
						:elitism 1
						:immigration 5
						:num-mutations 100
						:cross-fn ga/fixed-crossover}))


;for evolving actual levels,
(defn rand-gl-x []
	{:x (rand-int 200)})
(defn rand-gl-count []
	(+ 15 (rand-int 30)))
(defn rand-level-genotype [num]
	 (vec (sort-by :x (take num (repeatedly rand-gl-x)))))
(defn rand-gl []
	{:genotype (rand-level-genotype (rand-gl-count))})

(def *levels* (map dat-to-genotype
								(load-folder "res/out-blocks")
								(load-folder "res/out-enemies")))

;negative examples
(def *negative-levels*
	(take 30 (for [[distance num] (concat
																 (for [d [20 23 25 27 30]
																			 n [1 1 1 1 20]] [d n])
																 (for [d [2 3 4]
																			 n [1 1 1]] [d n])															 
																 [
;[15 15] [17 15] [20 15 [22 20]] [25 20] [27 20] [30 20] [32 30] [35 30]
																	])]
						 (vec (for [x (range 0 230 distance)
												rep (range num)] {:x x}))))
	#_(map scramble-level *levels*))

;to discourage the short-quick ones
#_(def *small-gamma-negative-levels*
	[]
	#_(repeatedly 10 (fn [] (vec (for [x (range 0 100 14) rep (range 2)] {:x x}))))
	#_(take 28 (repeatedly #(rand-level-genotype 5))))

;specially crafted to be the guys that would "exploit" the 40 big M range.
#_(def *big-gamma-negative-levels*
	[]
	#_(repeatedly 10 (fn [] (vec (for [x (range 0 230 30) rep (range 20)] {:x x}))))
	#_(map #(rand-level-genotype
				 (+ 100 (* 10 %))) (range 28))) ;100-370

;new negative example set of levels for our purposes!
#_(def *gamma-negative-levels* (concat *negative-levels*
																		 *small-gamma-negative-levels*
																		 *big-gamma-negative-levels*))
;a stratified segmentation. groups 2 and 1 are missing one positive example (due to 28 instead of 30 pos examples)
(def groupids [3 6 2 4 0 8 8 2 6 6 5 9 9 7 0 1 4 7 3 1 4 5 3 0 8 9 5 7 6 2 6 9 4 7 3 1 5 5 6 2 0 0 0 8 8 3 1 4 9 2 9 7 8 5 1 3 7 4])

;the old one from the first (incorrect run)
;(def groupids [4 6 6 0 4 9 8 2 2 4 7 8 3 5 4 1 0 7 7 2 3 9 7 3 6 8 5 0 3 1 4 3 6 6 9 3 2 5 0 5 5 0 1 8 1 8 1 4 9 7 6 2 9 8 9 2 5 1 7 0])
;a good level is {:set -1 :group 3 :fit -2343}
(def *total-level-set* (map #(assoc %1 :group %2)
														(concat (for [gt *levels*] {:set -1 :level gt})
																		(for [gt *negative-levels*] {:set 1 :level gt}))
														groupids))

;this is set to the n-1 sets for k-fold x-validation.
(def *current-level-set* (ref *total-level-set*))

(defn rand-gamma []
	{:genotype [(rand 29)
							(+ 2.0 (rand 10))
							(rand 2)
							1.0 #_(+ 0.5 (rand 1.0))]})

;-------------Population initialization---------------
(def generation (ref 0))
(def pop (ref (vec (take (:size @params) (repeatedly rand-gamma)))))
(def mutate-scale (atom 1.0))
;(swap! mutate-scale #(* % 0.1))

(defn gamma-mutate [{[T-window
											M
											m
											enem-scale] :genotype}]
	(let [mutate (fn [param scale]
								 (+ param (* @mutate-scale (rand-gauss scale))))]
		{:genotype [(clamp (mutate T-window 0.2) 0.01 29.0)
								(max 2.0 (mutate M 0.2))
								(clamp (mutate m 0.2) 0 5.0)
								(clamp (mutate enem-scale 1) 1 1)]})) 

;TODO: merge this with the mario one for less error-prone-ness
;problem in how I age, then measure, then add the new one... leaves too big of a gap...
(defn gamma-statehistory
	"The new journal-version fitness model for mario.
   takes model parameters and a list of challenge impulse events.
   impulse-events are [ ... {:x 3 :difficulty 234} ...]
   we can vary the model paramters as a function of t. (consider extracting this fn as a param.)"
	[{[T-window M m enem-scale :as model] :genotype} difficulty-events]
	(let [level-step (fn [{:keys [anxiety ready memory fun]},
												[dist challenge [T-window M m _]]]
										 (let [anx-to-fun #($= (-4) * ((% / (2 * M)) - 1) * (% / (2 * M)))
													 aged-memory (vec (filter
																						 #(< (first %) T-window)
																						 (map #(update-in % [0] + dist) memory)))
													 anxiety-in-window (reduce + (map second aged-memory))
													 cash-in? (and ready
																				 (<= anxiety-in-window m)
																				 (> dist 0.00001) ;epsion has passed since last impulse. (don't trigger a within-impulse rhyhtm group)
																				 )]
											 {:dist dist
												:cash cash-in?
												:aged-memory anxiety-in-window
												:anxiety (if cash-in?
																	 challenge
																	 (+ anxiety challenge))
												:memory (conj aged-memory
																			[0 challenge])												
												:ready (if (or (> (+ anxiety-in-window challenge) m)
																			 (> anxiety m)) ;I think...
																 true
																 (if cash-in?
																	 false
																	 ready))
												:fun (+ fun
																(if cash-in?
																	(anx-to-fun anxiety)
																	0))}))
				diff-tuples (sort-by first															; 
														 (concat [[0 0] [280 0] ] ;sential values, 2 at end to flush.
																		 (for [g difficulty-events]
																			 [(:x g) 1])))
				reducts (reductions level-step
														{:anxiety 0
														 :memory (list)
														 :ready false
														 :fun 0}
														(map vector
																 (separation (map first diff-tuples))
																 (drop 1 (map second diff-tuples))
																 (repeat model)))]
;flush out the "left-over" anxiety at the end of the level, regardless if there's a "natural" rhythm group end there.
		(concat reducts (list (level-step (assoc (last reducts) :ready true :memory [])
																			[0 0 model])))
		))

(defn gamma-statehistory-old
	"currently returns a state history vector.
   Memory is a failed concept, if I remember right.  it's the number
   of enemies in a fixed time in the past, regardless of rhyhtm
   groups. I used it as a way of dealing with slopes that are way to
   high (ten enemies in one location).  This is uncessarily specific
   though, because if we just have a reasonable difficulty measure,
   these will automatically be rejected (i.e. measure the effect of
   compound difficulty.)"
	[{[T-window M m enem-scale] :genotype} level-genotype]
	(let [anx-to-fun #($= (-4) * ((% / (2 * M)) - 1) * (% / (2 * M)))
				level-step (fn [{:keys [anxiety ready memory fun]}, [dist challenge]]
										 (let [anxiety-in-window
													 ,(reduce + (map second
																					 (take-while
																						#(< (first %) T-window)
																						(map #(update-in % [0] + dist) memory))))
													 cash-in? (and
																		 ready
																		 (<= anxiety-in-window m))]
											 {:anxiety (if cash-in?
																	 challenge
																	 (+ anxiety challenge))
												:memory (conj memory [0 challenge])
												:ready (if (> anxiety-in-window m)
																 true
																 (if cash-in?
																	 false
																	 ready))
												:fun (+ fun
																(if cash-in?
																	(anx-to-fun anxiety)
																	0))}))
				diff-tuples (sort-by first										; [[0 1], [1 1], [4 1] ...]
														 (concat [[0 0] [230 0] ] ;sential values, 2 at end to flush.
																		 (for [g level-genotype]
																			 [(:x g) (if (= :enemy (:type g))
																								 enem-scale
																								 1)])))]
		(reductions level-step
								{:anxiety 0
								 :memory (list)
								 :ready false
								 :fun 0}
								(map vector
										 (separation (map first diff-tuples))
										 (drop 1 (map second diff-tuples))))))
;todo: Have an enemy constant factor multiplier?

(defn gamma-level-fitness
	"the fitness of a particular level, given model params"
	[gamma-indiv level-genotype]
	(- (:fun (last (gamma-statehistory gamma-indiv level-genotype)))))


(defn gamma-fitness-sorted [gamma-indiv]
	(let [levels-new (map #(assoc % :fit (gamma-level-fitness gamma-indiv (:level %)))
										 @*current-level-set*)]
		(sort-by :fit (reverse (sort-by :set levels-new)))))

(defn gamma-fitness
	"This one measures sorting success, it doesn't care about the actual fun values.
   secondary sorting based on fitness distance."
	[gamma-indiv]
	(let [sorted (gamma-fitness-sorted gamma-indiv)
				both (map #(* -1 (:set %)) sorted) ;cuz I'm reversing
				halfsize (int (/ (count both) 2))
				pos-fit (map :fit (filter #(< (:set %) 0) sorted)) ;cuz I'm reversing these are -1 "good levels"
				neg-fit (map :fit (filter #(> (:set %) 0) sorted)) ;these are +1 "bad levels"
				pos-norm (/ (reduce + pos-fit) (inc (count pos-fit))) ;no div-by-zeros?
				neg-norm (/ (reduce + neg-fit) (inc (count neg-fit)))]
		(reduce +
						(concat
						 [pos-norm (- neg-norm)] ;secondary sorting
						 (map *
									both
									(map #(pow (- % halfsize) 3) (range))))))) ;edge ones are more important.

;old equivalent fitness
;(reduce + (map * (map :set (gamma-fitness-sorted (first @gamma-pop))) (range)))
(comment (defn gamma-fitness [gamma-indiv]
	"trying to get an average of 7ish rhythm groups per level"
	(abs (- 196 (reduce + (map
											 #(count (filter #{0}
																			 (map :anxiety (gamma-statehistory
																											gamma-indiv %))))
											 *levels*))))))

;-----------------------------------------------------------------------
;------------------------------------the GA-----------------------------
;-----------------------------------------------------------------------

(swap! params merge
			 {:mutate-fn gamma-mutate
				:rand-indiv rand-gamma})

(defn step []
 "a regular evolutionary step with 1 population."
  (dosync
	 (commute pop #(ga/sort-generation
												(ga/next-generation % @params)
												gamma-fitness))
	 (commute generation inc)
	 #_(swap! mutate-scale (fn [_] 
													 (pow 10 (- 1 (int (/ @gamma-generation 100))))))
	 #_(when (zero? (mod @generation 500))
		 (save-dat (str "res/out/gamma-" @generation ".txt") (first next)))
	 0))

;make this less repetitive (throw in some macros)

(def *runthread* (atom nil))

(defn stop []
  (. @*runthread* stop))

(defn start []
	(do
		(swap! *runthread* (fn [_] (ga/makethread step)))
		(. @*runthread* start)))

(defn reboot []
	(dosync
	 (ref-set generation 0)
	 (ref-set pop (vec (take (:size @params) (repeatedly rand-gamma))))
	 (swap! mutate-scale (constantly 1.0))))

;(prn @generation (float @mutate-scale) (update-in (first @pop) [:fitness] #(/ % 1000000)))
;(map :anxiety (gamma-statehistory (first @pop) (first *levels*))) ;show a run of a level
;(map #(count (filter #{0} (map :anxiety (gamma-statehistory (first @gamma-pop) %)))) *negative-levels*) ;count the number of "discharges"
;(map :set (gamma-fitness-sorted (first @gamma-pop))) ;for charting
;(map #(apply max (separation (map :x %))) *levels*) ;for separation
;(map #(gamma-level-fitness (first @gamma-pop) %) *levels*) ;for seeing fitness values

;number of rhythm groups:
;(map #(count (frequencies (map :fun (gamma-statehistory {:genotype [28 21 2.5 1]} %)))) *levels*)
;even better way
;(map #(get (frequencies (map :cash (gamma-statehistory model %))) true) *levels*)

;(map :fun (gamma-statehistory (first @pop) (first *levels*)))

;how to test a deviant:
;(defn deviant [num distance] (vec (for [x (range 0 230 distance) rep (range num)] {:x x})))

;how to get a list of random group-id assignments, for 58 individuals
;(shuffle (mapcat #(repeat (int (Math/ceil (/ 58 10))) %) (range 10)))

;28/30 split. need approximate stratification...
(shuffle (mapcat #(repeat (int (Math/ceil (/ 28 10))) %) (range 10)))
(shuffle (mapcat #(repeat (int (Math/ceil (/ 30 10))) %) (range 10)))



(defn count-inversions* [seq]
	(case (count seq)
				0 [0 seq]
				1 [0 seq]
				(let [n (int (/ (count seq) 2))
							[s1 s2] (map #(% n seq) [take drop])
							[i1 ss1] (count-inversions* s1)
							[i2 ss2] (count-inversions* s2)
							violate (reduce + (for [item ss1] (count (take-while #(< % item) ss2))))]
					[(+ i1 i2 violate) (sort (concat ss1 ss2))])))

(defn count-inversions [seq]
	(first (count-inversions* seq)))

(count-inversions [2 1 1 1 1 2 2 1 1 2 2 2 2 2])
(count-inversions [2, 4, 1, 3, 5]) ; 3
(count-inversions [3 1 4 2])
(count-inversions [1 3 4 2 5])
(count-inversions [1 5 4 8 10 2 6 9 12 11 3 7]) ;22

;mean inversions in size n is (n(n - 1))/4
;(* 6 5 0.25) we'd guess about 7.5 inversions on a random 6 group list.
;variance
;(Math/sqrt (/ (* 6 (+ (* 2 6) 5) 5) 72))
;variance of 2.661453237111885

(defn run-group
	[n]
	(dosync (ref-set *current-level-set* (filter #(not= n (:group %)) *total-level-set*)))
	(reboot)
	(doall (repeatedly 5 #(do (step) (Thread/sleep 100) (prn "partial: " n (first @pop)))))
	(Thread/sleep 100)
	(prn n '-> (first @pop)))

;to run k-fold cross validation
;(doall (map run-group (range 0 10)))

#_(def models-old
	[{:fitness -245053.69753456957, :genotype [6.665857915419166 5.622824846927701 1.7723715496835843 1]}
	 {:fitness -226249.56530535186, :genotype [6.038723566577084 6.107333818674934 2.3399013859903603 1]}
	 {:fitness -228965.9738648698, :genotype [6.275015671580311 5.3704851071852575 3.253802711045521 1]}
	 {:fitness -228290.3967696907, :genotype [6.859149853342462 5.218891431611841 3.9608518980971943 1]}
	 {:fitness -227899.24440542056, :genotype [6.573056994718937 5.049817466879188 1.4972797628666903 1]}
	 {:fitness -228069.27266263234, :genotype [7.337923414959866 7.96859049576066 1.9154585553443786 1]}
	 {:fitness -227960.26075514284, :genotype [6.232784414798191 5.046526396814112 0.4752806643298999 1]}
	 {:fitness -245048.5396401132, :genotype [6.001761559604923 4.971534146724092 0 1]}
	 {:fitness -227067.09847894136, :genotype [5.274185164551942 3.9810873209277116 0.8153312427147337 1]}
	 {:fitness -226979.8705516067, :genotype [6.993419401815124 4.998140902209949 0 1]}])

;only 2 misclassifications! on 0:(-1 -1 1 1 -1 1) and 9:(-1 -1 1 1 1 -1)
;out of 58 tests, that means 0.9655172413793103 correct!
;--average model [mean variance]
;[9.533725034734022 6.642184916333816]
;[7.291515845912938 3.463376229893015]
;[1.5138585994818756 0.22143582240349996]
(def models
	[
	 {:fitness -228764.39330711102, :genotype [11.012454662897206 8.139846568797555 1.4082522929509942 1.0]}
	 {:fitness -246369.8833202255, :genotype [10.887084140412833 7.985878659953309 1.2697402724677114 1.0]}
	 {:fitness -246388.64768310555, :genotype [10.858950262083136 9.079084883492177 1.6825715761466602 1.0]}
	 {:fitness -228759.7879418424, :genotype [10.756735695299048 7.998644987204351 1.871145924023131 1.0]}
	 {:fitness -228762.97596760403, :genotype [10.715040398757896 7.909860471069911 1.2673145266369432 1.0]}
	 {:fitness -228759.00085677108, :genotype [10.0306549548755 8.03018779366366 1.7078906764207409 1.0]}
	 {:fitness -228786.83752865606, :genotype [10.90830238840967 7.926806884491277 1.653262753444488 1.0]}
	 {:fitness -227415.87885955395, :genotype [3.3195099443994485 4.083626021703731 0.3858743228181518 1.0]}
	 {:fitness -228764.1198204721, :genotype [10.411626263274878 8.1903409556286 1.9770030503485507 1.0]}
	 {:fitness -228928.6534137868, :genotype [6.436891636930612 3.57088123312481 1.9155305995613865 1.0]}])

;evolved on entire set:
;{:fitness -353990.30997480307, :genotype [10.11111304632012 9.360918083226357 1.5313212870996087 1.0]}


(defn cross-validate
	[n]
	(dosync (ref-set *current-level-set* (filter #(= n (:group %)) *total-level-set*)))
	(count-inversions (map :set (gamma-fitness-sorted (get models n)))))


;to get average model
#_(map ((vector ((juxt mean variance) f)
							((juxt mean variance) s)
							((juxt mean variance) t))
			))

#_(map (fn [fun] (map :genotype models)) )

#_(for [selector [first second third]
			sequence (map selector (map :genotype models))]
	((juxt mean variance) sequence))

;((juxt mean variance) (map first (map :genotype models)))


;fourier analysis

;level to time series
(defn time-series
	"limited to 256 for now, just to make the FFT simpler."
	[level]
	(for [x (range 256)] (if (get (into #{} (map :x level)) x) 1 0)))

;(def ft (FourierTransform.))
;(.setData ft (double-array (time-series (first *levels*))))
;(.setData ft (double-array (for [x (range 260)] (if (= 0 (mod x 10)) 1 00)))) ;test set
;(.setSegmentNumber ft 50)
;(.setOverlapOption ft true)

;(.transform ft)
;(def results (map #(.getReal %) (.getTransformedDataAsComplex ft)))


;two dimensional... 256 long, 1/2 of 512 wich is my length.
;(count (first (.powerSpectrum ft)))
;(.getPowerSpectrumEstimate ft)
;(.plotPowerSpectrum ft)

(defn get-power-spectrum [time-series]
	(let [ft nil ;(FourierTransform.) 
				_ (.setData ft (double-array time-series))
				ps (.powerSpectrum ft)]
		(vec (second ps))))

(defn get-power-spectrum-level [level]
	(get-power-spectrum (time-series level)))

#_(def summed-power-spectrum (apply map + (map get-power-spectrum-level *levels*)))

;view the resulting power spectrum
#_(save-pdf
 (incanter.charts/xy-plot (range 2 128) (map #(* 1000 (/ % 28.0))
																						 (drop 2 summed-power-spectrum))
													:series-label "frequency"
													:x-label "frequency"
													:y-label "mean square amplitude")
 "charts/powerspectrum.pdf"						 
 :width 310
 :height 250)

#_(def ps2 (get-power-spectrum (smooth (map :x (first *levels*)) 4 256)))

;(view (incanter.charts/xy-plot (range 2 25) (drop 2 ps2)))

;(view (incanter.charts/xy-plot (range 2 25) (drop 2 ps2)))
;gaussian-filter
(defn set-nate-theme [myplot]
	(set-theme-bw myplot)
	(.setLabelPaint (.getRangeAxis (.getPlot myplot)) (new
																										 java.awt.Color 0 0 0))
	(.setLabelFont (.getRangeAxis (.getPlot myplot)) (new Font
																												"Serif" Font/PLAIN 18))
	(.setTickLabelFont (.getRangeAxis (.getPlot myplot)) (new Font
																														"Serif" Font/PLAIN 14))
	(.setTickLabelPaint (.getRangeAxis (.getPlot myplot)) (new
																												 java.awt.Color 0 0 0))
	(.setLabelPaint (.getDomainAxis (.getPlot myplot)) (new
																											java.awt.Color 0 0 0))
	(.setLabelFont (.getDomainAxis (.getPlot myplot)) (new Font
																												 "Serif" Font/PLAIN 18))
	(.setTickLabelFont (.getDomainAxis (.getPlot myplot)) (new Font
																														 "Serif" Font/PLAIN 14))
	(.setTickLabelPaint (.getDomainAxis (.getPlot myplot)) (new
																													java.awt.Color 0 0 0))
	myplot) 

;Mapping the fun response to anxiety
#_(save-pdf
 (-> (xy-plot [0 2.5] [0 0]
							:x-label "Anxiety"
							:y-label "Fun")
		 (add-function #($= (-4) * ((% / (2 * 1.0)) - 1) * (% / (2 * 1.0))) 0 2.5)
		 (incanter.charts/set-stroke :series 0 :width 2 :dash 5)
		 
		 (set-nate-theme)
		 )
 "charts/fun_response.pdf"
 :width 220
 :height 200)

;mapping the challenge events themselves

#_(save-pdf
 (let [points (for [i (range 7 #_(count *levels*))
										pts (map :x (nth *levels* i)) :when (< pts 200)] [pts (inc i)])]
	 (scatter-plot (map first points)
								 (map second points)
								 :x-label "Challenge Events"
								 :y-label "Level ID"))
 "charts/test_suite_partial.pdf"
 :width 700
 :height 220)

#_(save-pdf
 (let [points (for [i (range (count *negative-levels*))
										pts (map :x (nth *negative-levels* i)) :when (< pts 200)] [pts (inc i)])]
	 (scatter-plot (map first points)
								 (map second points)
								 :x-label "Challenge Events"
								 :y-label "Level ID"))
 "charts/test_suite_negative.pdf"
 :width 700
 :height 700)