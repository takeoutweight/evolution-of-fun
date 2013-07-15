(ns nathansorenson.nutils
  (:use [nathansorenson.priority-map])
  (:import  (java.util Random)
            (java.io File FileWriter BufferedReader FileReader)
            (clojure.lang PersistentTreeMap APersistentSet Reversible Sorted)))

(defn kill [] (System/exit 0))

(def Randgen (Random. 1234567))
(defn rand-gauss [scale] (* scale (.nextGaussian Randgen)))
(defn mutate [value scale] (+ value (rand-gauss scale)))
(defn rand-elt [col] (nth col (rand-int (count col))))

(defn third [x] (. clojure.lang.RT (third x)))
(defn fourth [x] (. clojure.lang.RT (fourth x)))

(defn separation
  "finds the distances between every consecutive enemy."
  [xs]
  (map #(- %2 %1) xs (rest xs)))

(defn assoc-apply
  "applies the fun (w/ takes one argument) to the map/vec at the key/index."
  [map, key, fun]
  (assoc map key (fun (map key))))

(comment (defn sorted-set-by [cmp & items]
  ((fn mkset [m #^PersistentTreeMap impl]
     (proxy [APersistentSet Reversible Sorted] [m impl]
       (disjoin [k] (if (contains? impl k) (mkset m (dissoc impl k)) this))
       (cons [k] (if (contains? impl k) this (mkset m (assoc impl k k))))
       (empty [] (mkset m (empty impl)))
       (rseq [] (.seq this false))
       (withMeta [m2] (mkset m2 impl))
       (comparator [] (.comparator impl))
       (entryKey [e] e)
       (seq ([] (keys impl))
            ([asc] (keys (.seq impl asc))))
       (seqFrom [k asc] (keys (.seqFrom impl k asc)))))
     nil (apply sorted-map-by cmp (interleave items items))))) 

(defmacro let->
  "Like ->, but takes a binding form as first argument, the bound
  symbol(s) from which will carry the resulting value from each form."
  ([[bind init] form]
    (if (seq? form)
      `(let [~bind ~init] ~form)
      (list form init)))
  ([[bind init] form & more]
    `(let-> [~bind (let-> [~bind ~init] ~form)] ~@more))) 

(defn to-string
  "serializes an object into a string."
  [obj]
  (binding [*print-dup* true] (pr-str obj)))

(defn from-str
  "deserializes an object from a string."
  [str]
  (with-in-str str (read)))
  
(defn str-to-file 
  "write a string to a text file"
  [str filename]
  (doto (FileWriter. (File. filename))
    (.write str)
    (.close)))

(defn file-to-str
  "read file contents into a string. TODO: Single line for now."
  [filename]
  (let [br  (BufferedReader. (FileReader. filename))
        str (.readLine br)]
    (.close br)
    str))
    
(defn clamp
  "clamps a value"
  [x imin imax]
  (min (max x imin) imax))
  
(defn between?
  "is a b'twixt min and max? Possibly equal to min, strictly less than max."
  [a min max]
  (and (< a max) (>= a min)))
  
(defn distance
  "distance between two blocks."
  [[ax ay][bx by]]
  (Math/sqrt (+ (Math/pow (- ax bx) 2) (Math/pow (- ay by) 2))))

(defn dist
  "manhattan distance. now only for two single values (not n-dim points)"
  [p1 p2]
  (Math/abs (- p1 p2)))

(defn lazy-or
  [seq]
  (if (empty? seq) 
    false
    (if (first seq)
        (first seq)
        (recur (rest seq)))))

(defn popfirst
  "like pop, but drops the first element of a vector instead of the last."
  [vec]
  (subvec vec 1 (count vec)))
      
(defn lazy-and
  [seq]
  (if (empty? seq) 
    true
    (if (not (first seq))
        (first seq)
        (recur (rest seq)))))

(defn compare-by [fun] (fn [a b] (compare (fun a) (fun b))))

(defstruct directed-graph
  :nodes ; The nodes of the graph, a collection
  :neighbors)

(defn get-neighbors
  "Get the neighbors of a node."
  [g n]
  ((:neighbors g) n))

(defn a-star
  "faster than loop/recur version.
   returns the path from start to end, including start and goal nodes.
   returns nil if no path is found."
  [start goal graph distfn]
  (let [init [{} (priority-map-by
                  (compare-by #(+ (:g %) (:h %)))
                  goal {:g 0 :h 0})]
        [visited _] ,
        (first
         (filter
          (fn [[_ open]] (nil? open))
          (iterate 
           (fn [[closed open]]
             (let [[ecoords :as expanded] (first open)
                   nclosed (conj closed expanded)]
               [nclosed,
                (when (and (not= start ecoords) start ecoords)
                  (merge-with
                   (fn [{og :g :as old} {ng :g :as new}]
                     (if (<= ng og) new old))
                   (dissoc open ecoords) 
                   (into {} (map #(vector
                                   % {:g (distfn ecoords %)
                                      :h (distfn % start)
                                      :came-from ecoords})
                                 (filter (complement nclosed)
                                         (get-neighbors graph ecoords))))))]))
           init)))]
    (let [path (take-while identity
                           (iterate
                            (fn [node] (:came-from (visited node))) start))]
      (if (= 1 (count path)) ;just the start is the "path", so no real path exists.
        nil
        path))))

(defmacro local-bindings
  "Produces a map of the names of local bindings to their values."
  []
  (let [symbols (map key @clojure.lang.Compiler/LOCAL_ENV)]
    (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))

(def ^:dynamic *locals*)
(defn eval-with-locals
  "Evals a form with given locals. The locals should be a map of symbols to
values."
  [locals form]
  (binding [*locals* locals]
    (eval
     `(let ~(vec (mapcat #(list % `(*locals* '~%)) (keys locals)))
        ~form))))

(defmacro debug-repl
  "Starts a REPL with the local bindings available."
  []
  `(clojure.main/repl
    :prompt #(print "dr => ")
    :eval (partial eval-with-locals (local-bindings))))
