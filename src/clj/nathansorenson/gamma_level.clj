;This evolves levels according to a gamma-prime model
;again we need to get using namespaces REAL SOON.
(comment



  
(ns nathansorenson.gamma-level)

(comment (def best-gamma-model-indiv ;NOT accounting for huge or small levels
     {:fitness -3.348316169741416,
      :genotype [9.815617364066593
                 2.8853414966244424
                 10.0
                 7.423014607614903
                 3.5588966070001367
                 2.4768378582613435
                 -0.21491405075039485]}))

(def best-gamma-model-indiv
     {:fitness -3831194.0,
      :genotype [22.008799425760593
                 217.06390084689488
                 3.566586820011879
                 12.811272530640336
                 0.5395076144014382
                 0
                -0.0792847901626447]})

;29 difficulties per level median
;200 max x position of last difficult thing median.

(def gl-generation (ref 0))
(def gl-popsize 60)
(def gl-elitism 1)
(def gl-immigration 5)

;level generation functions promoted to gamma-model.clj

(def *nummutations* 2000)
(def mutate-scale (atom 10.0))
(defn gl-mutate [{:keys [genotype] :as indiv}]
  "TODO: eventually needs to work on a keyed DE, not just on a number vec.
   but for now using just a number vec."
  (let [mutate (fn [x]
                 (clamp (+ x (* @mutate-scale (rand-gauss 1.0))) 0 200))]
    (update-in indiv
               [:genotype (rand-int (count genotype)) :x]
               mutate)))

(def gl-pop (ref (vec (take gl-popsize (repeatedly rand-gl)))))


(defn gl-fitness
  [gl-indiv]
  (gamma-level-fitness best-gamma-model-indiv (:genotype gl-indiv)))


(defn gl-step []
      "a regular evolutionary step with 1 population.
   relies on the next-generation fn to fix genotypes"
  (let [next
        (sort-generation
         (if (not (empty? @gl-pop))
           (next-generation
            @gl-pop
            gl-popsize
            gl-elitism
            gl-immigration
            gl-mutate
            crossover
            rand-gl)
           [])
         gl-fitness)]
    (dosync 
     (ref-set gl-pop next)
     (ref-set gl-generation (inc @gl-generation))
     (when (zero? (mod @gl-generation 500))
       (save-dat (str "res/out/gl-" @gl-generation ".txt") (first next)))
     0 )))

;*rebinding!*
;make this more extensible
(defn makethread [] 
  (proxy [Thread] []
    (run [] (loop [] (gl-step) (recur) ))))

;(prn @gl-generation (:fitness (first @gl-pop)) (count (:genotype (first @gl-pop))))
;(map :anxiety (gl-statehistory (first @gl-pop) (first *levels*))) ;show a run of a level
;(map #(count (filter #{0} (map :anxiety (gl-statehistory (first @gl-pop) %)))) *negative-levels*) ;count the number of "discharges"
)
