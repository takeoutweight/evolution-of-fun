;; prettyprinter
(ns nathansorenson.nutils
	(:refer-clojure))

(defmacro lazy-cons [x s]
  `(lazy-seq (cons ~x (lazy-seq ~s)))) 
  
(defn third [xs]
  (nth xs 2))

(defn flatten [x]
  (let [type (first x)]
    (cond (or (= type :NIL) (= type :TEXT))
          x
          (= type :CONCAT)
          (lazy-cat [:CONCAT] 
                    [(flatten (second x))]
                    [(flatten (third x))])
          (= type :NEST)
          (lazy-cat [:NEST] 
                    [(second x)]
                    [(flatten (third x))])
          (= type :LINE)
          (lazy-cat [:TEXT] [" "])
          (= type :UNION)
          (recur (second x)))))

(defn layout [x]
  (let [type (first x)]
    (cond (= type :Nil)
          ""
          (= type :Text)
          (lazy-cat (second x)
                    (layout (third x)))
          (= type :Line)
          (lazy-cat (lazy-cons \newline
                               (replicate (second x)
                                          \space))
                    (layout (third x))))))

(defn group [x]
  (lazy-cat [:UNION] [(flatten x)] [x]))

(defn fits [w x]
  (if (< w 0)
    false
    (let [type (first x)]
      (cond (= type :Nil)
            true
            (= type :Line)
            true
            (= type :Text)
            (recur (- w (.length (second x)))
                   (third x))))))

(defn better [w k x y]
  (if (fits (- w k) x)
    x
    y))

(defn be [w k d]
  (if (empty? d)
    ;(lazy-cons :Nil)
    (seq nil)
    (let [type (first (second (first d)))]
      (cond (= type :NIL)
            (recur w k (rest d))
            (= type :CONCAT)
            (let [level (first (first d))
                  doc (second (first d))]
              (recur w k (lazy-cons [level (second doc)]
                                    (lazy-cons [level (third doc)] (rest d)))))
            (= type :NEST)
            (let [level (first (first d))
                  doc (second (first d))]
              (recur w k (lazy-cons [(+ level (second doc))
                                     (third doc)]
                                    (rest d))))
            (= type :TEXT)
            (let [doc (second (first d))]
              (lazy-cat [:Text]
                        [ (second doc)]
                        [(be w (+ k (.length (second doc)))
                             (rest d))]))
            (= type :LINE)
            (let [level (first (first d))]
              (lazy-cat [:Line] [level]
                        [(be w level (rest d))]))
            (= type :UNION)
            (let [level (first (first d))
                  doc (second (first d))]
              (better w k (be w k (lazy-cons [level (second doc)] (rest d)))
                      (be w k (lazy-cons [level (third doc)] (rest d)))))))))

(defn best [w k x]
  (be w k (list [0,x])))

(defn pretty [w x]
  (layout (best w 0 x)))

(defn show-dispatch [x]
  (cond (seq? x)
        :list
        (vector? x)
        :vector
        (map? x)
        :map
        true
        :default))

(defmulti show show-dispatch)

(defn insert-line [x y]
  (lazy-cat [:CONCAT] [x] [(lazy-cat [:CONCAT] [(seq '(:LINE))] [y])]))


(defn show-children [x]
  (if (empty? x)
    (seq '(:NIL))
    (reduce insert-line (map show x))))


(defn make-show-brackets [lbrack rbrack]
  (fn [x]
    (lazy-cat [:CONCAT] [(lazy-cat [:TEXT] [lbrack])]
              [(lazy-cat [:CONCAT] [(lazy-cat [:NEST] [1] [(show-children x)])]
                         [(lazy-cat [:TEXT] [rbrack])])])))


(defn show-map-item [x]
  (lazy-cat [:CONCAT]
            [(show (first x))]
            [(lazy-cat [:CONCAT] [(lazy-cat [:TEXT] [" "])]
                       [(lazy-cat [:NEST] [(+ (.length (pr-str (first x)))
                                              1)]
                                  [(show (second x))])])]))
(def show-map-items)
(defn show-map-items [x]
  (cond (empty? x)
        (seq '(:NIL))
        (= (count x) 1)
        (show-map-item (first x))
        true
        (lazy-cat [:CONCAT]
                  [(show-map-item (first x))]
                  [(lazy-cat [:CONCAT] [(lazy-cat [:TEXT] [","])]
                             [(lazy-cat [:CONCAT]
                                        [(seq '(:LINE))]
                                        [(show-map-items (rest x))])])])))

(defn show-map-braces [x]
  (lazy-cat [:CONCAT] [(lazy-cat [:TEXT] ["{"])]
            [(lazy-cat [:CONCAT] 
                       [(lazy-cat [:NEST] [1] [(show-map-items (seq x))])]
                       [(lazy-cat [:TEXT] ["}"])])]))

(defmethod show :default [x]
  (lazy-cat [:TEXT] [(pr-str x)]))
(defmethod show :list [x]
  (group ((make-show-brackets "(" ")") x)))
(defmethod show :vector [x]
  (group ((make-show-brackets "[" "]") x)))
(defmethod show :map [x]
  (group (show-map-braces x)))
(defn pp
  ([obj width]
     (doseq [char (pretty width (show obj))]
       (print char)))
  ([obj]
     (pp obj 80))) 

