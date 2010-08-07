;!!!!! CAUTION! compiling this project actually builds a viewer window!
;(this is for my convenience)

(ns nathansorenson.zeldatwoviz
	(:use (nathansorenson zeldatwo))
	(:require [rosado.processing :as proc]
						[rosado.processing.applet :as proca]))

(def scale 30)
(defn draw-indiv [{:keys [genotype] :as indiv}]
	(doall (for [gene genotype]
					 (case (:type gene)
								 :room (let [[x y w h] (map #(* scale (gene %)) [:x :y :w :h])]
												 (proc/rect x y w h))
								 :door (let [[x y] (map #(int (* (/ scale index-to-actual) (gene %))) [:x :y])]
												 (case (:side gene)
															 :horiz (proc/rect (- x 2) (- y 3) 4 6)
															 :vert (proc/rect (- x 3) (- y 2) 6 4)
															 (proc/rect (- x 3) (- y 3) 6 6)))
								 :enemy (let [[x y] (map #(int (* (/ scale index-to-actual) (gene %))) [:x :y])]
													(proc/with-translation [x y]
														(proc/line -3 -3 3 3)
														(proc/line 3 -3 -3 3))) 
								 :nop))))

(def rand-cols (vec (repeatedly 50 #(vector
																		(+ 55 (rand-int 200))
																		(+ 55 (rand-int 200))
																		(+ 55 (rand-int 200))))))

(defn draw []	
	(proc/scale 1 -1.0)
	(proc/translate 0 -500)
	(proc/scale 0.9 0.9)
	(proc/framerate 2)
	(proc/background 50)
	(proc/no-fill)
	(proc/translate 10 10)
	(proc/stroke 35)
;--grid
	(doall (for [x (range 20)] (proc/line (* x scale) 0 (* x scale) 600)))
	(doall (for [y (range 20)] (proc/line 0 (* y scale) 600 (* y scale))))

	;the random infease population graphic
	(proc/translate 2 2)
	(proc/stroke 90 20 20)
	(draw-indiv (rand-nth @(pops-infease (rand-int num-islands))))
	(proc/translate -2 -2)
	
	(proc/stroke 120 30 30)
	(draw-indiv @cur-best-infeas)
	#_(doall (map #(do
								 (proc/stroke 20 (+ 50 (* % 10)) (+ 20 (* % 20)))
								 (draw-indiv (first @(nth pops %))))
							(range num-islands)))
	(proc/stroke 0 200 0)	
;(proc/fill 50 80 50)	
;(draw-indiv iddy)
	(draw-indiv @cur-best-feas)
	
;-- The fitness plots
	(proc/translate 3 450)	
	(proc/rect 0 0 10 10)
	
	(dorun (map-indexed
					(fn [i fps]
						(do
							(apply proc/stroke (rand-cols i))
							(proc/begin-shape)
							(let [[lastx _] (last @(nth fitplots i))
										scale (/ 400 (inc lastx))]
								(dorun (for [[x y] fps] (proc/vertex (* scale x) (* y 5)))))
							(proc/end-shape)))
					(map deref fitplots)))
;	
;(draw-indiv fiddy)
;(proc/translate 200 -200)
;(proc/stroke 0 130 250)
;(proc/rect 0 0 5 5)
;(doall (map draw-indiv [{:genotype (map #(hash-map :type :room :x % :y % :w 1 :h 1) (range 5))}]))
	#_((proc/begin-shape)
	(dorun (for [x (range 500)]
					 (proc/vertex x (* (* 4 90) (* (- (/ x (* 2 120)) 1)
																				 (/ x (* 2 120)))))))
	(proc/end-shape)
	(proc/rect 30 -30 3 3))
	)

(map #(swap! (nth fitplots %)
						 (fn [lst] (map (fn [[x y]] [(- x 1000) y]) lst)))
		 (range num-islands))

;as long as I keep there no constant, the x intercept will be zero.
;the max will always be at the zero derivative
;-4h*((x/2m)-1)(x/2m) ... for max m.


(proca/defapplet app :draw draw :size [500 500])
(defn show [] (proca/run app))
(show)
