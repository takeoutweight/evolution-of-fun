

;; leveleval
(ns nathansorenson.leveleval
	(:use [nathansorenson nutils flexencoding]
				[incanter core])
	(:refer-clojure)
	(:import (JaCoP.core Store IntVar Switches GoalVar)
					 (JaCoP.constraints XneqY)
					 (JaCoP.constraints.geost Geost GeostObject DBox Shape NonOverlapping)
					 (JaCoP.search DepthFirstSearch IndomainGoal)))

#_( I just set these via Java.
 (set! (. Switches traceStore) false)
 (set! (. Switches traceOperationsOnLevel) false)
 (set! (. Switches traceStoreRemoveLevel) false)
 (set! (. Switches traceIndexicals) false)
 (set! (. Switches traceSearch) false)
 (set! (. Switches traceSearchTree) false))

;A player state will be a map:
; :anxiety
;	:accomplishment
; :fun (implicit? a function of the previous two?)

(defn simplewalk?
	"Is it just a matter of walking left or right to get from this block
	to the next?"
	[[x1 y1][x2 y2]]
	(if (or
			 (and (= x2 (inc x1)) (= y2 y1))								;walking right
			 (and (= x2 (dec x1)) (= y2 y1))) ;walking left
		true
		false))
;(simplewalk? [0 0] [1 0]) ;should be true
;(simplewalk? [0 0] [2 0]) ;should be false
;(simplewalk? [0 0] [1 1]) ;should be false

;We will track how that changes over the course of the level
;jumps will be greedy to minimize anxiety
	
(def *max-footprint* 2) ;only count two spaces left and right.
(defn landing-footprint
	"determines the number of safe, 'simplewalkable' blocks reachable from the indexed one."
	[level, index]
	(let [search (fn [i, nextfun, numsafe, cnt]
									(if (or (zero? cnt)
													(not (between? (nextfun i) 0 (count level))) ;array bounds checking
												  (not (simplewalk? (nth level i) (nth level (nextfun i)))))
										numsafe
										(recur (nextfun i) nextfun (inc numsafe) (dec cnt))))]
		(+ 1																						;the block we're on, plus left and right.
			 (search index inc 0 *max-footprint*)
		   (search index dec 0 *max-footprint*))))	   
;(landing-footprint [[0 0] [1 0]], 0) ;should equal 2
;(landing-footprint [[0 0] [1 0] [2 0] [4 0]], 0) ; should equal 3			
;(landing-footprint '([0 0] [1 0]), 0) ;works on seqs

;we need the level for the landing footprints.
(defn jump-difficulty
	"returns difficulty between two indexed blocks. Fairly rough estimate for now.
	 Basically manhattan distance minus both our landing
	 footprints. (it's easier to jump from a wide platform too)" [level
	index1 index2]
	(let [feet (+ (landing-footprint level index1) (landing-footprint level index2))
				ydiff (Math/abs (- (second (nth level index1)) (second (nth level index2))))
				xdiff (Math/abs (- (first (nth level index1)) (first (nth level index2))))]
		(+ ydiff 
		   xdiff 
		   (* -1 feet) ;footprint is slightly more important (64.0 is blocksize)
		   10))) ;always add a constant value so jumps aren't ever easier than walking.

;(jump-difficulty '([0 0] [1 0] [2 0] [3 0] [4 0], [5,0] [6 0] [7 0] [8 0] [9 0]) 2 7) ;works on seqs
;range from 5ish to whatever manhattan distance is.

;start a little anxious as players need to be accustomed to surroundings

;(update-state {:anxiety -1 :isrelaxing true :fun 0 :boredom 0})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;The Player Model;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *boredom-limit* 7)
(def *anxiety-limit* 100)


(defn player-step-danger
	"given an event and current state,
   Updates internal state values according to model."
	[{:keys [x isrelaxing anxiety fun boredom]} difficulty]
	(let [
				new_anxiety (+ anxiety difficulty)
	
				;switch to recouperation mode when crossing the threshold.
				new_isrelaxing (if (> anxiety *anxiety-limit*)
											 		true
												  (if (< anxiety 0)
												  	false
												  	isrelaxing))
											 
				new_boredom 0
			
        ;now, provide fun if the state agrees with the difficulty
				new_fun (if isrelaxing
										(- fun difficulty)
										(+ fun difficulty))
				new_x x
				]
							  
		{:x new_x :anxiety new_anxiety :isrelaxing new_isrelaxing :fun new_fun :boredom new_boredom}))

(def *idle-difficulty* -1)
(defn player-step
	"Takes a previous state an an event. Calculates time between for idling.
   TODO: make this idling more robust, actually iterate if model changes"
	[{:keys [x isrelaxing anxiety fun boredom] :as state} 
	 {event-x :x event-difficulty :difficulty}]
	(assoc 
			(player-step-danger
			 (player-step-danger state (* *idle-difficulty* (- event-x x)))
			 event-difficulty)
		:x event-x))

(def best-gmodel
	(fn [t] [6.4 54 16]))

;level-length of 260ish? fe/level-length... Gives a pyramid distribution.
#_(defn varying-gmodel [t]
	(let [highpoint (* 0.5 @level-length)]
		[9.5,
		 (max
			0.49 ;cn't be zero, due to our formula.
			(if (< t highpoint)
				(* 7 (/ t highpoint))
				(* 7 (- 1 (/ (- t highpoint) (- @level-length highpoint)))))),
		 0.5] 
		))

;the constant one.
(defn varying-gmodel [t]
	(let [highpoint (* 0.5 @level-length)]
		[9.4,
		 6.0, 
		 0.5] 
		));<-- you have to be careful about this guy, if it's too high you can't ever trigger a rhythm group, and you could be severly penalized!

;Difficulty seems to be about 10 per hole and enemy. 8-12.
;TODO: merge this with the other statehistory that I cross-validate
(defn gamma-statehistory-mario
	"The new journal-version fitness model for mario.
   takes model parameters and a list of challenge impulse events.
   impulse-events are [ ... {:x 3 :difficulty 234} ...]
   we can vary the model paramters as a function of t. (consider extracting this fn as a param.)"
	[difficulty-events]
	(let [model-fun varying-gmodel  ;<----------where you pick the model params.
				level-step (fn [{:keys [anxiety ready memory fun]},
												[dist challenge [T-window M m]]]
										 (let [anx-to-fun #($= (-4) * ((% / (2 * M)) - 1) * (% / (2 * M)))
													 aged-memory (vec (filter
																						 #(<= (first %) T-window)
																						 (map #(update-in % [0] + dist) memory)))
													 anxiety-in-window (reduce + (map second aged-memory))
													 cash-in? (and ready
																				 (<= anxiety-in-window m)
																				 (> dist 0.00001) ;epsilon has passed since last impulse. (don't trigger a within-impulse rhythm group)
																				 )]
											 {:window anxiety-in-window
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
														 (concat [[0 0] [@level-length 0] ] ;sential values, 2 at end to flush.
																		 (for [g difficulty-events]
																			 [(:x g) (:difficulty g)])))
				reducts (reductions level-step
														{:anxiety 0
														 :memory (list)
														 :ready false
														 :fun 0}
														(map vector
																 (separation (map first diff-tuples))
																 (drop 1 (map second diff-tuples))
																 (map model-fun
																			(drop 1 (map first diff-tuples)))))]
;flush out the "left-over" anxiety at the end of the level, regardless if there's a "natural" rhythm group end there.
		;changed dist from 0 to 1 for the flush-out, because it was possibly ignored.
		(concat reducts (list (level-step (assoc (last reducts) :ready true :memory [])
																			[1 0 (model-fun @level-length)])))
		))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *jump-lookahead* 5)
(defn find-jumptarget 
	"Finds a target jump from the first block of the level.
	 Expects a vector of [x,y] coords as a level. not a genotype.
	 returns a vector of the index of the target block and its
	 difficulty."
	[blocklevel]
	(loop [ind (min *jump-lookahead* (dec (count blocklevel)))
				 min-index ind
				 min-difficulty 999999999]								
		(if (zero? ind)
			[min-index min-difficulty]
			(let [jd (jump-difficulty blocklevel 0 ind)]
				(if (< jd  min-difficulty)
					(recur (dec ind) ind jd)
					(recur (dec ind) min-index min-difficulty))))))

;(jump-difficulty [[0 0] [1 0]] 0 1) ;7
;(jump-difficulty [[0 0] [1 0] [3 0] [4 0]] 1 2)
;(find-jumptarget [[0 0] [100 0]]) ;1 970
;(find-jumptarget [[0 0] [100 0] [200 0]]) ;1 970
;(find-jumptarget [[0 0] [200 0] [100 0]]) ;2 970
;(jump-difficulty [[0 0] [75 0] [160 0]] 0 1) ;945
;(find-jumptarget [[0 0] [75 0] [160 0]]) ;1 945
;(find-jumptarget [[0 0] [160 0] [75 0]]) ;2 945
;(find-jumptarget (to-blocks [{:type :block :coords [4 4]} {:type :block :coords [200 65]}]))
;(find-jumptarget '([4 4] [200 65]))

(defn jump-events
	"Given a sorted vector of [x,y] blocks, Generate a list
 	 of events for every necessary jump."
	[block-level]
	(loop [evts [], lev block-level]
		(if (< (count lev) 2) 
			evts
			(if (simplewalk? (first lev) (second lev))
					 (recur evts, (rest lev))
					 (let [[ind diff] (find-jumptarget lev)]
						 (recur (conj evts {:type :jump :x (first (first lev)) :difficulty diff})
										(drop ind lev)))))))
;(jump-events '([0 0], [2 0] [3 0] [4 0], [8 0] [9 0]))

(def *default-enemy-difficulty* 10)
(defn enemy-events
	"given a flex-encoded genotype, generate a list of enemy events.
	 TODO: don't hard-code all enemy difficulty the same."
	[genotype]
	(map #(hash-map :type :enemy :x (:x %) :difficulty *default-enemy-difficulty*)
			 (filter #(or (= (:type %) :enemy) 
										(and (= (:type %) :pipe) (= (:piranha %) true)))
					genotype)))

;(enemy-events test-genotype)

(def *initial-state* {:x 0 :anxiety 10 :isrelaxing true :fun 0 :boredom 0})
(defn challenge-events
	" Now using gamma-statehistory, returns the statehistory."
	[genotype]
	(let [events (sort-by :x (concat (jump-events (flex-to-block-level genotype)) 
																	 (enemy-events genotype)))]
		events
		))

;(print (interleave (level-eval test-genotype) (repeat "\n")))

;difference from 6 switches
(defn count-switches "Deprecated. Use new gamma-model instead."
  [statehistory]
	(let [[switches _] (reduce
											(fn [[sum last] current]
												[(if (= current last)
													 sum
													 (inc sum)),
												 current])
											[0 :true]
											(map :isrelaxing statehistory))]
		switches))

;old, standard way (do I really not use this?)
(defn fitness-fun
  "Given an individual with a :genotype, 
   to be MINIMIZED... minimize the un-fun-ness."
  [challenge-events]
  #_(- ((peek (level-eval genotype)) :fun))
	#_(- (count-switches (level-eval genotype)))
	(- (:fun (last (gamma-statehistory-mario challenge-events)))))




;(fitness {:genotype [{:type :block :coords [4 4]} {:type :block :coords [63 65]} {:type :enemy} {:type :block :coords [100 65]}]})

(defn require-num
	"specifies constraint, how many of a type of gene you want. Returns 0-1
   with 1 being really far off of goal, and 0 being constraint met."
	[n type genotype]
	(let [error (Math/abs (- n (count (filter #(= (:type %) type) genotype))))]
		(/ error (+ 1 error))))

(defn require-no-overlap
	"gives a penalty for the number of these things overlapping.
   I happens to give no xs for ground and holes, so it should be okay
   to fold this over an entire genome w/o any filtering, since nothing
   really should overlap anyway.
   Relies on the fact that holes give empty lists of xs under get-xs.
   TODO: should make explicit which types ought not overlap.
   TODO: deal with problem of level-length being a soft bound.
   pipe, stairs, enemy, plat-raised are all defined in get-xs multimethod."
	[genotype]
	(let [increment-xs (fn [sumvec gene]
											 (reduce #(assoc-apply %1 %2 inc) sumvec (get-xs gene)))]
		(reduce + 
			(filter #(> % 0)
				(map dec
						 (reduce increment-xs (vec (repeat (+  @level-length 50) 0)) genotype))))))

(defn require-above-ground
	"adds up the guys that are above holes when they ought not be."
	[genotype]
	(let [holes (reify-holes genotype)
				guys-on-ground (filter #(#{:enemy :pipe :stairs} (:type %)) genotype)
				above-hole (fn [x] (if (holes x) 1 0))]
		(reduce + (for [gene guys-on-ground, x (get-xs gene)] (above-hole x)))))

(defn require-above-hole
	"adds up the guys that are above ground when they ought not be."
	[genotype]
	(let [holes (reify-holes genotype)
				guys-in-holes (filter #(#{:plat-raised} (:type %)) genotype)
				above-ground (fn [x] (if (holes x) 0 1))]
		(reduce + (for [gene guys-in-holes, x (get-xs gene)] (above-ground x)))))


(defn fitness-constraint
	"A test for constraints, accepts an individual.
   TODO: just returns 0 for now, everything is good!"
	[{genotype :genotype}]
	0)

(comment (+ (require-num 3 :enemy genotype)
																				;(require-num 2 :stairs genotype)
	 (require-num 2 :pipe genotype)
																				;(require-num 2 :plat-raised genotype)
																				;(require-num 5 :hole genotype)
	 (require-no-overlap genotype)
	 (require-above-ground genotype)
	 (require-above-hole genotype)))

(def *injected-genes* 
		 [;{:type :plat-raised :x 10 :height 3 :width 3}
			;{:type :plat-raised :x 13 :height 4 :width 3}
			;{:type :plat-raised :x 16 :height 3 :width 3}
			])


(defn feasible?
	[indiv]
	(= 0 (fitness-constraint indiv)))

(defn offset-pipes
	"certain genes need to be offset by one x to the right,
   such as pipes and cannons.
   TODO: put this somewhere more obvious... not a hidden postprocess step."
	[gene]
	(let [shift #(update-in gene [:x] inc)]
		(case (:type gene)
				:cannon (shift)
				:pipe (shift)
				:hole (shift)
				:enemy-pit (shift)
				:enemy-pit-above (shift)
				gene)))

(defn constrain-genotype
	"takes a genotype, uses JaCoP to fit constraints.
   TODO: I'm not sure where this function belongs best at this point.
   TODO: we're not explicitly stating these imports on this file yet
   TODO: a lot of the JaCoP integration could be streamlined (auto ID's eg.)"
	[flex-genotype]
	(let [store (Store.)
			variables (for [i (range (count flex-genotype))]
									(GoalVar. store (.concat "v" (.toString i)) 0, @level-length,
																 (:x (nth flex-genotype i))))
			widthvec (vec (map gene-to-width flex-genotype))
																				;bind varibles to shape objects:
			objects (reduce (fn [objlist variable]
												(conj objlist
															(GeostObject.
															 (count objlist) ;unique id
															 (into-array [variable]) ;origin
															 (let [w (nth widthvec (count objlist))]
																 (IntVar. store "shape" w,w))
															 (IntVar. store "start" 0,0)
															 (IntVar. store "dur" 1,1)
															 (IntVar. store "end" 1,1))))
											[]
											variables)
																				;[:type GeostObject.] pairs, for filtering purposes:
			typelist (map :type flex-genotype) ;vec of type names
			typed-objects (map vector typelist objects)
																				;pick only objects with certain type

				pick-objs (fn [obj-set]
										(map second (filter #(obj-set (first %))
																			 typed-objects)))
				non-overlap-sets [(pick-objs
														 #{:enemy
															 :hole
															 :pipe
															 :blocks
															 :blocks-with-enemies
															 :cannon
															 :enemy-pit
															 :enemy-pit-above
															 :enemy-row
															 :steps
															 :impediment
															 :hill-with-enemies})
													(pick-objs
													 #{:coin-arc
														 :coin-row
														 :pipe
														 :cannon
														 :hill
														 :hill-with-enemies})				
													(pick-objs
													 #{:coin-arc
														 :coin-row
														 :blocks
														 :blocks-with-enemies
														 :impediment})]
				
				varray (into-array variables)
      ;statefully impose constraints into store:
			;the array is which dimensions we want to worry about (i.e. 0th)	
				constraints (vec (map #(NonOverlapping. % (int-array [0]))
															non-overlap-sets))
			_ (.impose store (Geost. objects constraints shapes))
			result? (solve-store! store varray)]
																				;return:
		(vec
		 (map offset-pipes
					(map (fn [gene variable] (assoc gene :x (.value variable)))
							 flex-genotype
							 (map #(nth varray %) (range (count flex-genotype))))))))

(def default-constrain-timeout 2000)
(def min-constrain-timeout 1000)
(def max-constrain-timeout 5500)
(def constrain-timeout (atom nil))
(defn timed-constrain 
	"returns nil if we think we can't constrain this level.
   any successful constraints are used to tune the timeout
   value to 3x max time so far."
	[level-genotype]
	(let [start (System/currentTimeMillis)
						f (future (constrain-genotype level-genotype))
						c (try
								(.get f (if (nil? @constrain-timeout)
													default-constrain-timeout
													@constrain-timeout)
											java.util.concurrent.TimeUnit/MILLISECONDS)
								(catch java.util.concurrent.TimeoutException e
									nil)
								(catch java.lang.NullPointerException e ;not sure why I got this...
									nil)
								(catch java.util.concurrent.ExecutionException e
									nil)
								(catch java.lang.Exception e
									nil))
						end (System/currentTimeMillis)
						dur (- end start)]
				(when (not (nil? c))
					(swap! constrain-timeout
								 (fn [v] (if (nil? v)
													 (min (max min-constrain-timeout (* 3 dur)) max-constrain-timeout)
													 (min (max min-constrain-timeout v (* 3 dur)) max-constrain-timeout)))))
				c))

(defn provide-needed
	"provides a seq of the missing type"
	[n genotype type generatorfn]
	(let [c (count (filter #(= type (:type %)) genotype))
				diff (- n c)
				need (if (< diff 0) 0 diff)]
		(take diff (repeatedly generatorfn))))1

(defn add-minimum-genes
	"adds minimum required level elements."
	[flex-genotype]
	(let [need (fn [n t f] (provide-needed n flex-genotype t f))
				extra (concat
							 (need 3 :hole rand-hole)
							 (need 7 :enemy rand-enemy)
							 (need 5 :cannon rand-cannon)
							 (need 5 :pipe rand-pipe)
							 (need 3 :blocks rand-blocks))]
		(vec (concat flex-genotype extra))))

;(count (add-minimum-genes [{:type :hole}]))

;we convert everything to x-coord + difficulty values.
;paramenters *idle-difficulty* -1 --- is this "decay"?
;*anxiety-limit* is where we switch to not isrelaxing. (0 is fixed as little m.)
;*default-enemy-difficulty* = 10.

