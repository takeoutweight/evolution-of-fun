
;; ga

(ns nathansorenson.ga
	(:refer-clojure)
	(:use [nathansorenson
				 nutils flexencoding leveleval level-generator])
	(import (dk.itu.mario.engine PlayCustomized)
					(javax.swing JFrame SwingUtilities))
	)

(def *runname* "testrun")
(def *levelsize* 60)
(def *popsize* 5)
(def *num-mutations* 10)
(def *num-elite* 1)
(def *immigration* 1)
(def *tournament-size* 2)


;------------------------------------ The Genetic Operators ----------------------------
;(def testindiv {:genotype [[0,536][64,536][128,536][192,536][256,536]]})

;rand-block defined in deltaencoding.clj

(defn rand-individual
	"creates a completely random individual, of size 0-*levelsize*."
	[]
	{:genotype (vec (take (rand *levelsize*) (repeatedly rand-gene)))})
	
(defn rand-individuals [num]
	(vec (take num (repeatedly rand-individual))))
	
;mutate-gene defined in deltaencoding.clj

;Caution, we're getting g as a vector containing an individual,
;not an actual genotype. Something is super wrong somewhere.

(defn mutate-individual
	"mutates an individual. Removes nil'led genes "
	[individual]
	(let [g (:genotype individual)
				loc (rand-int (count g))]
		{:genotype (if (empty? g) 
								 [] 
								 (vec (remove nil? (assoc-apply g loc mutate-gene))))}))

(defn variable-crossover
	"crosses-over two individuals."
	[{genes1 :genotype :as parentA} {genes2 :genotype} prob]
	(if (< (rand) prob)
		(let [size1 (count genes1)
					size2 (count genes2)
					point1 (rand-int size1)
					point2 (rand-int size2)
					first-genes1 (subvec (vec genes1) 0 point1)
					second-genes2 (subvec (vec genes2) point2 size2)]
			{:genotype (vec (concat first-genes1 second-genes2))})
		parentA))

(defn fixed-crossover
	"fixed-genotype size crossover"
	[{genes1 :genotype :as parentA} {genes2 :genotype} prob]
	(if (< (rand) prob)
		(let [point (inc (rand-int (dec (count genes1))))
					first-genes1 (subvec genes1 0 point)
					second-genes2 (subvec genes2 point (count genes2))]
			{:genotype (vec (concat first-genes1 second-genes2))})
		parentA))

;------------------------The Fitness Function-----------------------------

;was in deltaencoding.clj
;now defined in leveleval.clj

;------------------------The Genetic Algorithm-----------------------------

;NOTE: this can be parallelized... but it seems to interact poorly with the constraint solver somehow... can hang the system (deadlock somewhere?)
(defn calc-fitness 
	"associates fitness values to the population. Returns a seq."
	[pop fit-func]
	(map #(assoc % :fitness (fit-func %)) pop))

;Nils are put before everything else, highest numbers are put on left
;assumes the fitness values have been attached
(defn sort-pop
	"returns a seq, sorted population."
	[pop]
  ;associate a fitness with each individual in population, then sort on that.
  (vec (sort-by :fitness pop)))

(defn mutate-pop
	"returns a mutated population..."
	[pop mutate-individual num-mutations]
  (let [size (count pop)]
		(if (> size 0)
			(let [mutate #(update-in %
															 [(rand-int size)]
															 mutate-individual)]
				#_(.println System/out (str "mutating " num-mutations " guys."))
				(nth (iterate mutate pop) num-mutations))
			pop)))
 
 (defn rand-tri
  "picks a kind-of-triangular number mode of zero, for crossover picks. Equiv to tournament selection"
  [popsize]
  (apply min (take *tournament-size* (repeatedly #(rand-int popsize)))))
   		
 (defn newpop-via-crossover 
 	"generates a new population of n individuals based on crossovers from n. Assumes pop is sorted vector."
 	[pop n cross-fn prob]
	(loop [ipop []]
	        (if (>= (count ipop) n)
	          ipop
	          (recur (conj ipop (cross-fn 
	                              ;pick individuals to breed based on triangle dist.
	                              (pop (rand-tri (count pop))) 
	                              (pop (rand-tri (count pop)))
																prob))))))

(defn inject-genes-into-indiv
	"Forcibly injects user-specified genes into every genotype.
   Does not preserve fitness."
	[{genotype :genotype} genes]
	{:genotype (vec (concat genes genotype))})

(defn inject-genes-into-pop
	[pop genes]
	(vec (map #(inject-genes-into-indiv % genes) pop)))

;TODO: depends on *injected-genes* being defined somewhere else.

(defn repair-indiv
	[{genotype :genotype}]
	{:genotype (vec (constrain-genotype
									 (add-minimum-genes genotype)))})

(defn repair-pop
	[pop]
	(vec (map repair-indiv pop)))

(def default-params
		 {:size *popsize*
			:elitism *num-elite*	 
			:immigration *immigration*
			:mutate-fn (fn [indiv] indiv)
			:num-mutations *num-mutations*
			:cross-fn fixed-crossover
			:crossover 0.9
			:rand-indiv (fn [] {:genotype []})})

(defn next-generation
	"breeds the next generation. Assumes it's sorted according to fitness.
   Takes a parameter map, but all keys have default values if a partial
   map is provided."
	([pop] (next-generation pop default-params))
	([pop param-map]
		 (if (empty? pop)
			 []
			 (let [{:keys [size elitism
										 immigration mutate-fn
										 num-mutations cross-fn
										 rand-indiv crossover]} (merge default-params param-map)
										 newpop (-> pop
																(newpop-via-crossover (- size elitism immigration) cross-fn crossover)
																(mutate-pop mutate-fn num-mutations)						 
																(into (take immigration (repeatedly rand-indiv)))
;(inject-genes-into-pop *injected-genes*)
;(repair-pop)
																)]
				 (vec (concat (take elitism pop) newpop))))))	;ensure elite are still at the front.

(defn sort-generation 
	[pop fit-func]
	(-> pop
			(calc-fitness fit-func)
			(sort-pop)))

(defn default-step! [pop generation fitness-indiv params]
	"provide REFERENCES for the pop and generation, values for others."
	(dosync
	 (commute pop #(sort-generation
												(next-generation % params)
												fitness-indiv))
	 (commute generation inc)))



;------------------------------- Thread Implementation and Utilities -----------------------

;(defn show-stats []
;	(make-window "Stats" (chart/line "Fitness" "generation" "fitness" @stats)))

(defn currentbest [] nil)

;(def a (generate-level (constrain-genotype (:genotype (rand-individual)))))

;dumpfile saves best individual to a smclvl file

;this is hook that is rebound in window.clj TODO: better modularity
(defn update-image [])
      
(comment (defn printpop []
  "prints the fitness values of a population nicely"
  (doseq [item @*pop-infeasible*]
    (println (format "fit: \t%d" (:fitness item))))))

(comment (defn save-stats 
  "dumps the current best's fitness into a file"
  [] 
  (do
    (dosync (ref-set stats (assoc @stats @generation ((currentbest) :fitness))))
    (str-to-file (to-string @stats) "stats.txt"))))

(defn makethread [step-fn] 
  (proxy [Thread] []
    (run [] (loop [] (step-fn) (recur) )))) ;switch this with zstep to do zelda.
