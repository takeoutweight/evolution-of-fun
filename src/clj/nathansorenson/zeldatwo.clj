(ns nathansorenson.zeldatwo
  (:refer-clojure)
  (:use [incanter.core :only [$=]]
        [nathansorenson.gamma-model :only [set-nate-theme]])
  (:require [nathansorenson.ga :as ga]
            [nathansorenson.nutils :as nu]
            [nathansorenson.levelreader :as lr]
            [nathansorenson.server :as sv]
            [clojure.set :as set]
            [fogus.me.trammel :as tram]
            [incanter.core :as in]
            [incanter.charts :as inc]
            [incanter.pdf :as inp]
            )
  (:import (JaCoP.core Store GoalVar)
           (JaCoP.constraints XneqY)
           (JaCoP.constraints.geost Geost GeostObject DBox Shape NonOverlapping)
           (JaCoP.search DepthFirstSearch IndomainGoal InputOrderSelectByVar)
           (java.io BufferedReader InputStreamReader OutputStreamWriter)
           ))

;----------------------------------------------------------------------
;---- DE defs ---------------------------------------------------------
;----------------------------------------------------------------------


(def genotype-size 75)
(def index-to-actual 16) ;blocks per section.
(defn actual-to-index [n] (int (/ n index-to-actual)))

(def room-penatly-threshold 20)
;(def entry [15 2])
;(def exit [15 28])
(def entry [5 2])
(def exit [5 8])

;(def entry [1 10])
;(def exit [36 10])

;note "max" is not <= but strict <. (like "range" etc...)
;generally 10 and 4.
;the big one was 30 and 6
;my mixed-init one is 38 18, by 5
(def x {:type :int :min 0 :max 10})
(def y {:type :int :min 0 :max 10})
(def w {:type :int :min 1 :max 4})
(def h w)
(def real-x {:type :int :min 0 :max (* (+ (:max x) (:max w) -1) index-to-actual)})
(def real-y {:type :int :min 0 :max (* (+ (:max y) (:max h) -1) index-to-actual)})

(def genes
     {:room {:x x :y y :w w :h h}
      :door {:x real-x :y real-y}
      :enemy {:x real-x :y real-y}})

;----------------------------------------------------------------------
;---- type contracts  -------------------------------------------------
;----------------------------------------------------------------------

(defn gene? [g]
  (and (map? g)
       (not (nil? (:x g)))
       (not (nil? (:y g)))))

(defn indiv? [i]
     (and (map? i)
          (not (nil? (:genotype i)))
          (vector? (:genotype i))
          (zero? (count (filter false? (map gene? (:genotype i)))))))

(defmacro returns
  "Installs the same postcondition check on every arglist"
  [f retfn]
  (let [argls (:arglists (meta (resolve f)))
        conts (interleave argls (repeat :ensures) (repeat retfn))
        body (list* f conts)]
   `(tram/apply-contracts [~@body])))

;----------------------------------------------------------------------
;---- genetic operations  ---------------------------------------------
;----------------------------------------------------------------------

;25 genes, here, obv genotype size must be larger.
;4x4 is my maximum, you have to change the bounds in w and h or else you'll crash this
(def *fixed-genes* [] #_[{:type :door, :x 144, :y 168.0} {:type :door, :x 208, :y 168.0} {:type :door, :x 240, :y 168.0} {:type :door, :x 264.0, :y 192} {:type :door, :x 264.0, :y 224} {:type :door, :x 288, :y 248.0} {:type :door, :x 320, :y 248.0} {:type :door, :x 344.0, :y 224} {:type :door, :x 344.0, :y 192} {:type :door, :x 368, :y 168.0} {:type :door, :x 400, :y 168.0} {:type :door, :x 448, :y 168.0} {:type :room, :x 9, :y 9, :w 4, :h 4} {:type :room, :x 13, :y 10, :w 2, :h 1} {:type :room, :x 15, :y 9, :w 3, :h 3} {:type :room, :x 16, :y 12, :w 1, :h 2} {:type :room, :x 15, :y 14, :w 3, :h 3} {:type :room, :x 18, :y 15, :w 2, :h 1} {:type :room, :x 20, :y 14, :w 3, :h 3} {:type :room, :x 21, :y 12, :w 1, :h 2} {:type :room, :x 20, :y 9, :w 3, :h 3} {:type :room, :x 23, :y 10, :w 2, :h 1} {:type :room, :x 25, :y 9, :w 4, :h 4} ])

#_(def myrooms [{:type :room :x 1,:y 10,:w 1,:h 1}
               {:type :room :x 9,:y 9,:w 4,:h 4}
               {:type :room :x 13,:y 10,:w 2,:h 1}
               {:type :room :x 15,:y 9,:w 3,:h 3}
               {:type :room :x 16,:y 12,:w 1,:h 2}
               {:type :room :x 15,:y 14,:w 3,:h 3}
               {:type :room :x 18,:y 15,:w 2,:h 1}
               {:type :room :x 20,:y 14,:w 3,:h 3}
               {:type :room :x 21,:y 12,:w 1,:h 2}
               {:type :room :x 20,:y 9,:w 3,:h 3}
               {:type :room :x 23,:y 10,:w 2,:h 1}
               {:type :room :x 25,:y 9,:w 4,:h 4}
               {:type :room :x 36,:y 10,:w 1,:h 1}])

#_(def mydoors-indicies
  [{:type :door :x 9, :y 10.5}
   {:type :door :x 13, :y 10.5}
   {:type :door :x 15, :y 10.5}
   {:type :door :x 16.5, :y 12}
   {:type :door :x 16.5, :y 14}
   {:type :door :x 18, :y  15.5}
   {:type :door :x 20, :y 15.5}
   {:type :door :x 21.5, :y 14}
   {:type :door :x 21.5, :y 12}
   {:type :door :x 23, :y 10.5}
   {:type :door :x 25, :y 10.5}
   {:type :door :x 28, :y 10.5}])



(def count-fixed (count *fixed-genes*))

;----------------------------------------------------------------------
;---- genetic operations  ---------------------------------------------
;----------------------------------------------------------------------

;TODO: make a probability distr. for gene selection.
;TODO: promote this stuff to the ga.clj engine, if it seems generic enough.
;      (would require a "genes" global var to hold the variable classes).
(defn rand-gene []
  (let [pick (fn [[Key GP]]
               [Key
                (case (:type GP)
                      :int (+ (rand-int (- (:max GP) (:min GP))) (:min GP)))])
        [type gps] (condp > (rand)
                     0.5  [:door (:door genes)]
                     0.75 [:enemy (:enemy genes)]
                     [:room (:room genes)])]
    (into {:type type} (map pick (dissoc gps :type)))))

(def *mutation-scale* 1.0)
(defn mutate-gene
  "alters the parameters of a DE gene, w/o changing the type."
  [gene]
  (let [gene-params (genes (:type gene))
        [key p] (rand-nth (seq gene-params))
        mutate-val (fn [[K p]]
                     [K,
                      (case (:type p)
                            :int (int (nu/clamp (nu/mutate
                                                  (K gene)
                                                  (* 0.1 *mutation-scale*
                                                     (- (:max p) (:min p))))
                                                (:min p) (dec (:max p)))))])]
    (into gene (map mutate-val gene-params))))

(defn delete-rand-gene [genotype]
  (let [c (count genotype)
        n (+ count-fixed (rand-int (- c count-fixed)))]
    (if (zero? (- c count-fixed))
      genotype
      (vec (concat (subvec genotype 0 n) (subvec genotype (inc n) c))))))


;TODO: make an "Add new gene" to counteract the downward pressure.
;took out the "change for a new gene : #(assoc-in % [(rand-int (count %))] (rand-gene))
(def *new-gene-prob* 0.3)
(defn mutate-indiv 
  "mutates a gene, changes a gene type to a random new gene, or deletes a gene."
  [indiv]
  (let [add-new #(conj % (rand-gene))
        mutate-op (condp > (rand)
                    *new-gene-prob* delete-rand-gene
                    (* 2 *new-gene-prob*) add-new
                    #(let [c (- (count %) count-fixed)]
                       (if (> c 0)
                         (update-in % [(+ count-fixed (rand-int c))] mutate-gene)
                         (add-new %))))]
    (assoc (update-in indiv [:genotype] mutate-op)
      :backup (or (:backup indiv) (:genotype indiv)))))

;----------------------------------------------------------------------
;---- GA helpers  -----------------------------------------------------
;----------------------------------------------------------------------

(defn filter-type
  "returns a filter function that selects on the type of gene"
  [type seq]
  (filter #(= type (:type %)) seq))

(defn room-spaces [genotype]
  "returns a set of index coords for the spaces covered by a genotype.
   seems to work about as fast as room-hash. (which makes sense)"
  (let [spaces (fn [gene] (for [x (range (:x gene) (+ (:x gene) (:w gene)))
                                y (range (:y gene) (+ (:y gene) (:h gene)))]
                            [x y]))]
    (reduce (fn [acc gene] (into acc (spaces gene)))
            #{}
            (filter-type :room genotype))))

(defn room-hash
  "a spatial index, gives the room gene where you are, takes indexes,
   not real coords."
  [genotype]
  (let [spaces (fn [gene] (for [x (range (:x gene) (+ (:x gene) (:w gene)))
                                y (range (:y gene) (+ (:y gene) (:h gene)))]
                            [x y]))]
    (reduce (fn [acc gene] (reduce #(assoc %1 %2 gene) acc (spaces gene)))
            {}
            (filter-type :room genotype))))


(defn enemy-hash
  "returns a map that, given a room gene, gives the number of enemies in it.
   the nil key is the number of enemies not in any room."
  ([genotype] (enemy-hash genotype (room-hash genotype)))
  ([genotype roomhash]
     (reduce (fn [acc {:keys [x y]}]
               (update-in acc [(roomhash (map actual-to-index [x y]))]
                          #(if % (inc %) 1)))
             {}
             (filter-type :enemy genotype))))

;NOTE: we go room-room connections... we could go door-door connections... think about it.
(defn impose-link
  "makes connections in the adjacency map for a door"
  [adj-map room-hash door]
  (let [[ix iy] (map #(int (/ (% door) index-to-actual)) [:x :y])
        [ix2 iy2] (if (= :horiz (:side door)) [(dec ix) iy] [ix (dec iy)])
        room1 (room-hash [ix iy])
        room2 (room-hash [ix2 iy2])]
    (if (and room1 room2)
      (-> adj-map (update-in [room1] set/union #{room2})
                  (update-in [room2] set/union #{room1}))
      adj-map))) 

(defn room-graph
  "nodes are the actual genes."
  [genotype]
  (struct-map nu/directed-graph
    :nodes (filter #(= :room (:type %)) genotype)
    :neighbors (reduce #(impose-link %1 (room-hash genotype) %2)
                       {}
                       (filter-type :door genotype))))

;manhattan distance between room centres.
(defn room-dist [r1 r2]
  (+ (Math/abs (double (- (+ (:x r1) (/ (:w r1) 2))
                  (+ (:x r2) (/ (:w r2) 2)))))
     (Math/abs (double (- (+ (:y r1) (/ (:h r1) 2))
                  (+ (:y r2) (/ (:h r2) 2)))))))

;"just an empirical measurement, the average distance contribution between genes.
;   It seems pretty accurate (low variance), when looking at distance between randomly
;   generated indivs."
(def bottom-dist 46.6351) 

(defn gene-dist
  "TODO: we're normalizing manually; make the normalization inferred from the genes."
  [gene-A gene-B]
  (let [tA (:type gene-A)
        tB (:type gene-B)
        d (fn [k] (nu/dist (k gene-A) (k gene-B)))]
    (if (not= tA tB)
      bottom-dist
      (case tA
            (:enemy, :door) (+ (d :x) (d :y))
            :room (+ (d :x) (d :y)
                     ;(* 2.5 (d :w)) ;hunch that these are less relevant right now....
                     ;(* 2.5 (d :h))
                     )
            bottom-dist))))

(defn chamfer-dist
  "chamfer distance between two genotypes
    TODO: what if chamfer was the min? would that help with the size dominance?"
  [genotype-A genotype-B]
  (let [sA (count (filter-type :room genotype-A))
        sB (count (filter-type :room genotype-B))
        [gA gB] (if (< sA sB)
                  [genotype-A genotype-B]
                  [genotype-A genotype-B])]
    (reduce + (for [t [:room] ;:enemy :door...
                    a (filter-type t gA)]
                (let [m (for [b (filter-type t gB)]
                          (gene-dist a b))]
                  (if (empty? m)
                    bottom-dist
                    (apply min m)))))))

(def cd2 (fn [gA gB]
               (reduce + (for [t [:room] ;:enemy :door...
                               a (filter-type t gA)]
                           (let [m (for [b (filter-type t gB)]
                                     (gene-dist a b))]
                             (if (empty? m)
                               bottom-dist
                               (apply min m)))))))

; 96% of the time, the smaller chamfer distance is with the smaller (:room type only) genotype...
;but something must have been wrong with my testing, as it seems right 100% of the time when implementend.
;(frequencies (map (juxt #(> (cd2 %1 %2) (cd2 %2 %1)) #(> (count (filter-type :room %1)) (count (filter-type :room %2)))) (repeatedly 1000 rand-genotype) (repeatedly 10000 #(rand-genotype (rand-int 40)))))

;(room-dist {:x 0 :y 0 :w 2 :h 1} {:x 1 :y 0 :w 2 :h 2})
;(sample both sides for connectivity in graph construction)
;(def graddy (room-graph giddy))
; a single room is a depth of 1. 
(defn count-depth [graph node]
  (count (take-while identity
          (iterate (fn [s]
                     (let [newset (into s (mapcat #(nu/get-neighbors graph %) s))]
                       (if (= newset s)
                         nil
                         newset)))
                   #{node}))))

;NOTE: we're doing a sum now... We're subtracting one since we don't want
;  (seemed to reward huge rooms...
(defn genotype-depth
  "finds the maximum distance we walk walk from room to room."
  [genotype]
  (if (> (count (filter-type :room genotype)) 0)
    (let [g (room-graph genotype)]
      (reduce + (map #(dec (count-depth g %)) (filter-type :room genotype))))
    0))

(defn fix-door
  "Snaps a door to the closest room-edge boundary.
   TODO: might break under possibly negative x values
   due to rounding. ALSO assigns which side room is on."
  [{:keys [x y] :as gene} room-hash]
  (let [half (/ index-to-actual 2)
        ix (int (/ x index-to-actual))
        iy (int (/ y index-to-actual))
        {rx :x ry :y rw :w rh :h} (room-hash [ix iy])]
    (if (nil? rx)
;(assoc gene :delete true) <--- to delete a gene if it doesn't overlap a room
      gene
      (let [sx0 (* index-to-actual rx)
            sx1 (* index-to-actual (+ rx rw))
            sy0 (* index-to-actual ry)
            sy1 (* index-to-actual (+ ry rh))       
            delta [[:x (Math/abs (- x sx0)) sx0 :horiz]
                   [:x (Math/abs (- x sx1)) sx1 :horiz]
                   [:y (Math/abs (- y sy0)) sy0 :vert]
                   [:y (Math/abs (- y sy1)) sy1 :vert]]
            [dim _ val side] (first (sort-by second delta))]
        (into gene [[dim val] [:side side]])))))

;(def rh (room-hash (:genotype iddy)))
;(rh [(int (/ 33 index-to-actual)) (int (/ 65 index-to-actual))])
;(fix-door2 {:x 34 :y 65} rh)
;FIXME: it doesn't behave well if the door is outside any room.

(defn simple-repair
  "makes any easy constraint repairs that /could/ be handled by the
   CSP solver, but it's just as easy to do ourselves."
  [indiv]
  (let [fix (fn [gene] 
              (case (:type gene)
                    :door (fix-door gene (room-hash (:genotype indiv)))
                    gene))]
    (update-in indiv [:genotype] #(vec (filter (fn [g] (nil? (:delete g)))
                                               (map fix %))))))


;can't rely on "area" as max one side... we need to go up to MAX x MAX
(def shapeseq
     (filter (fn [[W H]] (and (< W (:max w)) (< H (:max h))))
             (for [A (range (:min w) (inc (* (:max w) (:max w)))),
                   W (range 1 A)]
               [W (- A W)])))

(def shapemap (into {} (map vector shapeseq (range))))

;we could have a collection of >1 dboxes to have complex shapes
(def shapes
     (let [make-shape (fn [i [W H]]
                        (Shape. i [(DBox. (int-array [0 0])
                        (int-array [W H]))]))]
         (map-indexed make-shape shapeseq)))

;we need to ensure we search in a reasonable way (not focus entirely on X first, but on XY manhattan dist.)
;NOTE: doesn't have one-dimensional shapes (w but no h). Probably never need them.
;NOTE: JaCoP uses <= min/max ranges, (inclusive) so be cautious!
(defn solve-constraints
  "run the individual through the actual JaCoP CSP solver."
  [{:keys [genotype] :as indiv}]
  (if (<= (count (filter-type :room genotype)) 1)
    indiv
    (let [store (Store.)
        makevar (fn [min max ideal]
                  (GoalVar. store "var", min, max, ideal))
        var-origin (fn [Gene] 
                     (map #(let [vardef ((genes (:type Gene)) %)]
                             (makevar (:min vardef)
                                      (dec (:max vardef))
                                      (Gene %)))
                          [:x :y]))
        var-shape (fn [Gene]
                    (makevar 0 (dec (count shapeseq)) (shapemap [(:w Gene) (:h Gene)])))
        relevant-genes (filter #(= :room (:type %)) genotype)
        var-list (for [g relevant-genes] [(var-origin g) (var-shape g)]) ;<---g is null
        objects (map-indexed (fn [id, [orig shape]] (GeostObject. id
                                          (into-array orig),
                                          shape
                                          (makevar 0 0 0)
                                          (makevar 1 1 1)
                                          (makevar 1 1 1)))
                             var-list)
        constraints [(NonOverlapping. objects (int-array [0 1]))]
        _ (.impose store (Geost. objects constraints shapes))
        dfs (DepthFirstSearch.)
        _ (.setPrintInfo dfs false) 
        result? (.labeling dfs store
                           (InputOrderSelectByVar.
                            store
                            (into-array (flatten var-list))
                            (IndomainGoal.))) ;<- the magic slection procedure.
        fix (fn [gene [[x-var y-var] shape-var]]
              (let [[w h] (nth shapeseq (.value shape-var))]
              (-> gene
                  (assoc :x (.value x-var))
                  (assoc :y (.value y-var))
                  (assoc :w w)
                  (assoc :h h))))]
    
    (assoc indiv :genotype
           (first (reduce (fn [[new-genotype solved-vs] gene]
                            (if (= :room (:type gene))
                              [(conj new-genotype
                                     (fix gene (first solved-vs))),
                               (rest solved-vs)]
                              [(conj new-genotype gene), solved-vs]))
                          [[] var-list]
                          genotype))))))

(def constraints-timeout 500)

(defn timed-constraints-local
  "if there isn't a backup, we don't need to re-solve it. We assume it's good.
   If there is a backup, and we can solve it, delete the backup cuz we're good.
   Otherwise, revert to the backup and forget this ever happened."
  [indiv]
  (if (:backup indiv)
    (let [f (future (solve-constraints indiv))
          solved (try
                   (.get f constraints-timeout
                         java.util.concurrent.TimeUnit/MILLISECONDS)
                   (catch java.util.concurrent.TimeoutException e
                     nil)
                   (catch java.lang.NullPointerException e ;not sure why I got this...
                     nil)
                   (catch java.util.concurrent.ExecutionException e
                     nil)
                   (catch java.lang.Exception e
                     nil))]       
      (if solved
        (dissoc solved :backup)
        (assoc indiv :genotype (:backup indiv))))
    indiv))

(def timed-constraints timed-constraints-local)
;(def timed-constraints sv/post-job)
;(println "CAUTION!!! You're using distributed constraint solving.")


(defn gamma-step
  "tuning so it works on traversals through our maze."
  [{:keys [anxiety fun]} num-enemies]
  (let [h 1
        m 5
        anx-to-fun #($= (-4 * h) * ((% / (2 * m)) - 1) * (% / (2 * m)))]
    (let [es? (> num-enemies 0)]
      {:anxiety (if es? (+ anxiety num-enemies),
                    0)
       :fun (if es? fun,
                (+ fun (anx-to-fun anxiety)))})))

;alt symbolic: (2x/m)-(x^2/m^2), deriv: 2/m-(2 x)/m^2
;(:fun (reduce gamma-step {:anxiety 0 :fun 0} enemy-per-room-list))
;[{:type :room, :x 5, :y 0, :w 1, :h 5} {:type :room, :x 5, :y 5, :w 1, :h 6} {:type :enemy, :x 80, :y 144} {:type :door, :x 81, :y 80, :side :vert}]


(def crazy-door-grid (for [x (range 8 (:max real-x) index-to-actual)
                           y (range 8 (:max real-y) index-to-actual)]
                       {:type :door :x x :y y}))
;(dosync (commute (pops-infease 0) vec))
;(dosync (dorun (map #(commute (pops-infease 0) update-in [% :genotype] into crazy-door-grid) (range 20))))

;debug versions
#_(defn fitness
  [{:keys [genotype] :as indiv}]
  (Math/abs (- (count genotype) 5)))

#_(defn feasibility
  [{:keys [genotype] :as indiv}]
  (Math/abs (- (count genotype) 5)))

;this is not a good thing to use. Call the minimization function directly.
(def minimize-genotype (atom false))
(defn fitness [{:keys [genotype] :as indiv}]
  (or (when (not (:backup indiv)) (:fitness indiv)) ;don't recalc if you don't need to.
      (let [rg (room-graph genotype)
            rh (room-hash genotype)
            eh (enemy-hash genotype rh)
            enemy-per-room-list (map #(get eh % 0) (nu/a-star (rh exit)
                                                              (rh entry)
                                                              rg room-dist))]
        (double
         (+ (- (:fun (reduce gamma-step {:anxiety 0 :fun 0} enemy-per-room-list)))
            (let [c (count (filter-type :room genotype))]
              (if (> c room-penatly-threshold) (/ (- c room-penatly-threshold) 10) 0))
            #_"the 10 is subtracted to prefrence minimizers over the old non-minimizers"
            (if @minimize-genotype (- (/ (count genotype) 1000) 10) 0)
            ;(- 1000)
            )))
      
      #_(double (+ (- (genotype-depth (:genotype i)))
                   (let [c (count (:genotype i))]
                     (if (> c 75) (/ (- c 75) 1000)
                         0)) ;penalize excessively large genotypes. 75 seems to grind things to a halt.
                   (/ (count (filter-type :room (:genotype i))) 1000)))))

(def cur-best-feas (atom {:fitness 10000 :feas 10000 :genotype []}))
(def cur-best-infeas (atom {:fitness 10000 :feas 10000 :genotype []}))

;not using tabu list for now.
;(def tabu-list (atom []))

;commenting out tabu for now.
(defn extended-fitness
  "extended with tabu information. yes. No."
  [{:keys [genotype] :as indiv}]
  (+ (fitness indiv)
     #_(/ (reduce + (map #(count (set/intersection (room-spaces genotype)
                                                 (room-spaces (:genotype %))))
                       (conj @tabu-list @cur-best-feas))) 1000)
     ))

;chamfer ; (- (/ (reduce + (map #(chamfer-dist (:genotype indiv) (:genotype %)) (conj @tabu-list @cur-best-feas))) 1000))

;commenting out tabu list for now.
(defn feasibility
  "0 if there is no problems. >0 if there are constraint violations
   (Higher is worse)"
  [{:keys [genotype] :as indiv}]
  (let [rhash (room-hash genotype)
        rgraph (room-graph genotype)
        start (rhash entry)
        end (rhash exit)]
    (double (+ (if start 0 100)
               (if end 0 100)
               (if (and start end (nu/a-star start end rgraph room-dist)) 0 100)
               #_(- (/ (reduce + (map #(chamfer-dist (:genotype indiv)
                                                     (:genotype %))
                                      (conj @tabu-list @cur-best-feas))) 1000000))
               (float (/ (-
                          (+ (count-depth rgraph start)
                             (count-depth rgraph end))) 1000))
               (float
                (/ (-
                    (let [room-nodes (for [{:keys [x y]} (filter-type :room genotype)] (rhash [x y]))]
                      (reduce + (map #(count-depth rgraph %) room-nodes)))) 100000))
               #_"the following won't work, will it? I mean, we need nodes not genes.."
               #_(- (/ (reduce + (for [r (filter-type :room genotype)]
                                   (if (or (nu/a-star r start rgraph room-dist)
                                           (nu/a-star r end rgraph room-dist))
                                     1
                                     -1))) 10000))
               #_(/ (reduce + (map #(count (set/intersection (room-spaces genotype)
                                                             (room-spaces (:genotype %))))
                                   (conj @tabu-list @cur-best-feas))) 10000)
               (- (/ (count (filter-type :door genotype)) 100000))
               (- (/ (count (filter-type :room genotype)) 10000))))))

;----------------------------------------------------------------------
;---- GA --------------------------------------------------------------
;----------------------------------------------------------------------

(defn rand-genotype
  ([]
     (vec (concat *fixed-genes* (repeatedly (- genotype-size count-fixed) rand-gene))))
  ([n]
     (vec (concat *fixed-genes* (repeatedly (- n count-fixed) rand-gene)))))



(defn rand-indiv []
  (solve-constraints {:genotype (rand-genotype)}))

(defn anti-crossover "just picks parent A."
  [i_1 i_2]
  i_1)

(def *crossover-prob* 0.8)
(defn geo-crossover
  "cuts genes genographically, horizontally or vertically. Removes duplicates.
   NOW with a X% chance of crossover."
  [{g1 :genotype :as indiv-A} {g2 :genotype :as indiv-B} prob]
  (condp > (rand)
    prob (let [[unfixed-g1 unfixed-g2] (map #(drop count-fixed %) [g1 g2])
               minnil #(if (or (nil? %1) (nil? %2))
                         nil
                         (min %1 %2))
               [dim cutpoint],
               (rand-nth
                [[:x (rand-int (:max real-x))]
                 [:y (rand-int (:max real-y))]])
               splitfn (fn [compare]
                         #(case (:type %)
                                :room (compare (* index-to-actual (dim %))
                                               cutpoint)
                                (compare (dim %) cutpoint)))]
           {:genotype (vec (concat
                            *fixed-genes*
                            (vec (into #{} (concat (filter (splitfn <) unfixed-g1)
                                                   (filter (splitfn >=) unfixed-g2))))))
            :backup (or (:backup indiv-A) (:genotype indiv-A))
            :fitness (minnil (:fitness indiv-A) (:fitness indiv-B))})
    indiv-A))

(def params
     (atom {:size 20
            :elitism 1
            :immigration 1            
            :mutate-fn mutate-indiv
            :num-mutations 50
            :cross-fn geo-crossover ;anti-crossover ;ga/crossover
            :crossover 0.7
            :rand-indiv rand-indiv}))

;this can't be changed during a run. I think you probably have to reboot VM entirely.
(def num-islands 1)

(def param-variations (map #(into @params {:num-mutations %})
                           (cycle [160 140 60 40]) )) ;(range 80 0 -7)
;I think crossover-prob is now useless, as it's pushed into the regular params.
(def thread-variations (map #(hash-map #'*crossover-prob* %1
                                       #'*new-gene-prob* %2
                                       #'*mutation-scale* %3)
                            (cycle [0.1 0.7 0.75 0.8 0.85]) ;(range 0 1.1 0.1)
                            (cycle [0.3 0.35 0.4 0.45]) ;(range 0.5 0 -0.05)
                            (cycle [0.4 0.5 0.6 0.7]) ;(cycle [0.33 0.66 1 2])
                            ))

(defn correct-pop-size [pop]
  (vec (take (:size @params) (concat pop (repeatedly rand-indiv)))))

;(def feasible-pop (ref (correct-pop-size [])))

;pops is a vector of REFS to populations.
;Q: should we stick all these into a nested map for santiy?
(def pops (vec (repeatedly num-islands #(ref [])))) ;empty
(def pops-infease (vec (repeatedly num-islands #(ref (correct-pop-size [])))))
(def gens (vec (repeatedly num-islands #(atom 0))))
(def gens-infeas (vec (repeatedly num-islands #(atom 0))))
(def fitplots (vec (repeatedly num-islands #(atom (vector [0.1 0.1])))))
(def fitplots-infeas (vec (repeatedly num-islands #(atom (vector [0.1 0.1])))))

(defn intermingle
  "currently moves the top half of each population over to the next.
   If you intermingle with an empty pop, you kill off individuals..."
  [refs]
  (let [vals (doall (for [r refs] (deref r))) ;grab vals before any migrations
        migrate (fn [& seqs] ;note we don't reduce size to previous... see if it works..
                  (vec (loop [acc []
                              ss seqs]
                         (if (every? empty? ss)
                           (flatten acc) ;basically an interpose, but takes longer seq.
                           (recur (conj acc (map first (keep not-empty ss)))
                                  (map rest ss))))))]
    (dorun (map (fn [r neighb]
                  (dosync (commute r migrate neighb)))
                refs
                (drop 1 (cycle vals)))))) ;migrate from the pop on your "left"

(def generation (atom 0))
(def fitplot (atom [[0.1 0.1]]))
(def intermingle-generations (* num-islands 700)) ;on average every n generations, do a island migration.
(def intermingle-countdown (atom 10))

(defn repair-pop [pop]
  (->> pop
       (map timed-constraints)
       (map simple-repair)
       (map simple-repair) ;to avoid the appearance of "drops" when doors are pushed off edges. (twice is the fixpoint-ish)
       (map #(update-in % [:genotype] vec)))) ;also, is this necessary?

;Ingress population needs to survive on its own merits.
;TODO ... next generation should have the pop at the END, not the front.
;         as it is a collection. Should be ammenable to the map ops, not the key ops.
;this is where the call to the actual GA happens, 
(defn next-gen-infeas
  "returns [expel-pop, keep-pop].
   ejects feasible individuals"
  [pop ingress params]
  (let [<n #(if (not (or (nil? %1) (nil? %2))) ;less than that can handle nils.
              (< %1 %2)
              (if (not (nil? %1))
                true
                false))
        feas-comp (comparator #(if (= (:fitness %1) ;primarily by fitness, then by feas.
                                      (:fitness %2))
                                 (< (:feas %1)
                                    (:feas %2))
                                 (<n (:fitness %1)
                                     (:fitness %2))))
        newpop (-> pop
                    (vec)
                    (ga/next-generation params)
                    (concat ingress)
                    (repair-pop))       
        grouped (group-by #(<= (:feas %) 0) 
                          (map #(assoc % :feas (feasibility %))
                               newpop))
        expel (grouped true)
        keep (sort feas-comp (grouped false))]
    [expel keep]))

;(ga/sort-generation feasibility)

;Erases fitness because we cache it in the fitness function. We need to force a recalc
;because we're switching from fitness-as-feasibility to "real" fitness.
;This is probably a brittle way to do it... (if there's trouble, just recalc fitness always)
(defn next-gen
  "returns [expel-pop keep-pop].
   checks for feasibility first, ejects those that fail.
   Associates 'real' fitness to the :fitness key, but sorts by
   fitness+chamfer distance for niching."
  [pop ingress params]
  (let [[fease infease] (next-gen-infeas pop ingress params)]
    [infease
     (take 20 (->> (concat (map #(dissoc % :fitness) fease) pop ingress) ;keep old generation around, in case we destroy the new one.
                  (map #(assoc % :fitness (fitness %)))
                  (sort-by extended-fitness)))]))


(def history (atom []))

(def expel-feas (vec (repeatedly num-islands #(ref (list)))))
(def expel-infeas (vec (repeatedly num-islands #(ref (list)))))

(defn step-infeas
  "returns a regular evolutionary step closure for an infeasible population"
  [id] 
  (fn []
    (with-bindings (nth thread-variations id)
      (let [[expel keep] (next-gen-infeas @(pops-infease id)
                                          @(expel-feas id)
                                          (nth param-variations id))]
        (dosync (ref-set (pops-infease id) keep)
                (commute (expel-infeas id) into expel)
                (commute (expel-feas id) empty))))
    
    (let [g (swap! (nth gens-infeas id) inc)
          best (first (deref (nth pops-infease id)))]
      (when best
        (swap! (nth fitplots-infeas id) conj [g, (:feas best)])
        (swap! cur-best-infeas (fn [old] (if (< (:feas best)
                                                (:feas old))
                                           (into best {:gen g :island id})
                                           old)))))))

(defn stepfn 
  "a regular evolutionary step with a certain population.
   relies on the next-generation fn to fix genotypes.  we have
   sanctioned GA parameters and our own ad-hoc thread-local binding
   parameters we can change.
   TODO: remove reporting from here into its own section."
  [id]
  (fn []    
    (do
      (let [inc? (if (zero? (count @(pops id)))
                   (do (Thread/sleep 500)
                       false)
                   true)] ;slow thread down until there are feasible guys.
      
        (with-bindings (nth thread-variations id)
          (let [[expel keep] (next-gen @(pops id)
                                       @(expel-infeas id)
                                       (nth param-variations id))]
            (dosync (ref-set (pops id) keep)
                    (commute (expel-feas id) into expel)
                    (commute (expel-infeas id) empty))))        

        (when inc?
          (let [g (swap! (nth gens id) inc)
                best (first (deref (nth pops id)))]     
            (when best
              (swap! (nth fitplots id) conj [g, (:fitness best)])
              (swap! cur-best-feas (fn [old] (if (< (:fitness best)
                                                    (:fitness old))
                                               (into best {:gen g :island id})
                                               old))))
      
            (when (<= (swap! intermingle-countdown dec) 0)
              (do (swap! intermingle-countdown (fn [_] intermingle-generations))            
                  #_(intermingle pops)
                  #_(intermingle pops-infease)
                  (lr/save-dat "cur-best.txt" @cur-best-feas)
                  #_(swap! history conj [((juxt :gen :fitness :island) @cur-best-feas) (.getTime (java.util.Calendar/getInstance))])
                  (prn :feas [((juxt :gen :fitness :island) @cur-best-feas) (.getTime (java.util.Calendar/getInstance))])))))))))

(def runthreads (vec (repeatedly num-islands #(atom nil))))
(def runthreads-infease (vec (repeatedly num-islands #(atom nil))))

(defn stop []
  (dorun (for [t (concat runthreads runthreads-infease)] (. @t stop))))

(defn start []
  (dorun
   (for [n (range num-islands)]
     (do
       (swap! (nth runthreads n) (fn [_] (ga/makethread (stepfn n))))
       (. @(nth runthreads n) start)
       (swap! (nth runthreads-infease n) (fn [_] (ga/makethread (step-infeas n))))
       (. @(nth runthreads-infease n) start)))))

;----------------------------------------------------------------------
;---- Type Checking  --------------------------------------------------
;----------------------------------------------------------------------

;these maybe aren't working as well as I could have hoped...
; eg solve-constraints throws it... but it seems to work okay?
(defn type-check
  []
  (do
    (returns mutate-indiv indiv?)
    (returns rand-indiv indiv?)
    (returns simple-repair indiv?)
;(returns solve-constraints indiv?) ; seems to work in reality...
    (returns timed-constraints indiv?)      ;maybe a source of error?
    (returns timed-constraints-local indiv?) ;maybe a source of error?
    (returns geo-crossover indiv?)

    (returns rand-gene gene?)
    (returns mutate-gene gene?)
    (returns fix-door gene?)))


;----------------------------------------------------------------------
;---- Misc  -----------------------------------------------------------
;----------------------------------------------------------------------



;[(:gen @cur-best) (:fitness @cur-best) (:island @cur-best) (.getTime (java.util.Calendar/getInstance))]

#_(def iddys (repeatedly 1000 rand-indiv))
#_(def bad (into []
      (filter
       #(let [i (simple-repair
                 (timed-constraints
                  %))]
          (not (= (genotype-depth (:genotype i))
             (genotype-depth (:genotype (simple-repair i))))))
       iddys)))

(defn transpose [seqseq]
  (loop [a [] b seqseq]
    (if (empty? (first b))
      a
      (recur (conj a (vec (map first b)))
             (map rest b)))))

(defn reboot []
  (do 
    (stop)
    (def cur-best-feas (atom {:fitness 10000 :feas 10000 :genotype []}))
    (def cur-best-infeas (atom {:fitness 10000 :feas 10000 :genotype []}))
    ;(def tabu-list (atom []))
    (def pops (vec (repeatedly num-islands #(ref [])))) ;empty
    (def pops-infease (vec (repeatedly num-islands #(ref (correct-pop-size [])))))
    (def gens (vec (repeatedly num-islands #(atom 0))))
    (def gens-infeas (vec (repeatedly num-islands #(atom 0))))
    (def fitplots (vec (repeatedly num-islands #(atom (vector [0.1 0.1])))))
    (def fitplots-infeas (vec (repeatedly num-islands #(atom (vector [0.1 0.1])))))
    (def generation (atom 0))
    (def fitplot (atom [[0.1 0.1]]))
    (def intermingle-generations (* num-islands 700)) ;on average every n generations, do a island migration.
    (def intermingle-countdown (atom 10))
    (def history (atom []))
    (def expel-feas (vec (repeatedly num-islands #(ref (list)))))
    (def expel-infeas (vec (repeatedly num-islands #(ref (list)))))
    (def runthreads (vec (repeatedly num-islands #(atom nil))))
    (def runthreads-infease (vec (repeatedly num-islands #(atom nil))))))


(defn constraint-client
  "turns this instance into a constraint-solving client."
  [socket]
  (sv/client socket timed-constraints-local))

(defn inc-client
  "just for testing"
  [socket]
  (sv/client socket inc))
;-------------------------------------------------
(defn reducerate
  "like reduce, but operates repeatedly on itself until it returns nil.
   returns the last value before returning nil."
  [fun arg]
  (last (take-while identity (iterate fun arg))))

(defn minimize
  "removes all nonessential genes from a genotype"
  [{:keys [genotype] :as indiv}]
  (let [fit (fitness indiv)
        test-gene (fn [[gt-keep [gene & gt-rest]]]
                    (when gene
                      (if (> (fitness {:genotype (vec (concat gt-keep gt-rest))})
                             fit)
                        [(conj gt-keep gene) gt-rest]
                        [gt-keep gt-rest])))]
    (assoc indiv :genotype
           (first (reducerate test-gene [[] genotype])))))

;--------------------------------------------------
(defn chart-zelda-indiv [indiv]
  (in/view (-> (inc/xy-plot (range 0 200) (range 0 200))
               (inc/add-polygon [[10 10] [10 100] [100 100] [100 10]])
               #_(set-nate-theme)
               )))


(defn chart-rect! [chart x y w h]
  (inc/add-polygon chart [[x y]
                          [(+ x w) y]
                          [(+ x w) (+ y h)]
                          [x (+ y h)]]))
(def x-radius 1.5)
(def chart-scale 10)
(defn chart-x! [chart x y]
  (let [x-rad (* x-radius 0.7)]
    (do
      (inc/add-polygon chart [[(- x x-rad) (- y x-rad)]
                              [(+ x x-rad) (+ y x-rad)]])
      (inc/add-polygon chart [[(- x x-rad) (+ y x-rad)]
                              [(+ x x-rad) (- y x-rad)]]))))

(defn chart-door! [chart x y side]
  (case side
        :horiz (chart-rect! chart
                            (- x (/ x-radius 2))
                            (- y x-radius)
                            x-radius
                            (* 2 x-radius))
        (chart-rect! chart
                     (- x x-radius)
                     (- y (/ x-radius 2))
                     (* 2 x-radius)
                     x-radius)))
;entry and exit
(defn chart-indiv [{:keys [genotype]}]
  (let [chart (inc/scatter-plot [-40 480] [-20 240] ;-20 540 for the big guy
                                :x-label ""
                                :y-label "")]
    (doall (for [gene genotype]
             (case (:type gene)
                   :room (let [[x y w h] (map #(* chart-scale (gene %)) [:x :y :w :h])]
                           (chart-rect! chart x y w h))
                   :door (let [[x y] (map #(int (* (/ chart-scale index-to-actual) (gene %))) [:x :y])]
                           (chart-door! chart x y (:side gene)))
                   :enemy (let [[x y] (map #(int (* (/ chart-scale index-to-actual) (gene %))) [:x :y])]
                            (chart-x! chart x y)) 
                   :nop)))
    (let [[entry-x entry-y exit-x exit-y] (map #(* chart-scale (+ % 0.5)) (concat entry exit))]
      (inc/add-text chart entry-x entry-y "A")
      (inc/add-text chart exit-x exit-y "B"))
    (set-nate-theme chart)
    (.setRangeGridlinePaint (.getPlot chart) java.awt.Color/white)
    (.setDomainGridlinePaint (.getPlot chart) java.awt.Color/white)
    chart))

(defn view-chart-indiv [indiv]
  (in/view (chart-indiv indiv) :width 500 :height 500))

(defn pdf-chart-indiv [indiv filename-root]
  (inp/save-pdf (chart-indiv indiv) (str "level_saves/" filename-root ".pdf") :width 1000 :height 500 ))


(defn save-evo-data [filename-root]
  (let [evo-data {:cur-best-feas @cur-best-feas
                  :cur-best-infeas @cur-best-infeas
                  :fitplots (map deref fitplots)
                  :fitplots-infeas (map deref fitplots-infeas)}]
    (nu/str-to-file (nu/to-string evo-data) (str "level_saves/" filename-root ".txt"))))

;to load again:
#_(nu/from-str (nu/file-to-str "level_saves/dogtest.txt"))

(defn save-all-stats []
  (let [best @cur-best-feas
        root (str "g" (:gen best) "_"
                  "i" @(first gens-infeas) "_"
                  (first (str (:fitness best))) ;clumsy way of taking first 2 chars of fitness val
                  (second (str (:fitness best))))]
    (do
      (pdf-chart-indiv (minimize best) root)
      (save-evo-data root))))

;to view the progress
#_[@(first gens-infeas) ((juxt :fitness :gen :genotype) @cur-best-feas)]

;to begin minimization
(defn toggle-minimize [] (swap! minimize-genotype not))

;to add an indiv to the fit population
#_(dosync (commute (first pops) conj b))

;to change the best guy with your own guy for vizualizing
#_(swap! cur-best-feas (fn [_] {:genotype *fixed-genes*}))

#_(do
  (dosync (commute
           (first pops-infease)
           (fn [pop]
             (map (fn [{g :genotype}]
                    (map  g)) pop))))
  :hey)

#_(conj pop
                   (update-in (first pop)
                              [:genotype]
                              conj {:type :room :x 7 :y 13 :w 3 :h 2}))

#_(def old2 (assoc old :genotype (vec (map (fn [gene]
                           (if (= (:x gene) 7)
                             (assoc gene :w 2)
                             gene)) (:genotype old)))))

#_(dosync (commute (first pops-infease) (fn [p] (repeat (count p) old2))))
