(ns nathansorenson.level-evolve
	"evolves DE levels with the new Constraint-solver fixup technique."
	(:refer-clojure :exclude [pop])
	(:require [nathansorenson
						 [ga :as ga]
						 [gamma-model :as gm]
						 [level-generator :as lg]
						 [flexencoding :as fe]
						 [leveleval :as leval]
						 [imageutils :as im]]
						[clojure.test :as test]
						[clojure.repl :as repl])
	(:use [nathansorenson nutils]
				[incanter core pdf charts])
	(:import [dk.itu.mario.engine PlayCustomized LevelRenderer Art]
					 [dk.itu.mario.MarioInterface Constraints]
					 [javax.swing SwingUtilities]
					 [java.awt AlphaComposite Color GraphicsEnvironment
						         GraphicsDevice GraphicsConfiguration]
					 [java.awt.image BufferedImage]
					 [java.util Calendar]
					 [java.text SimpleDateFormat]					 
					 ))

;It doesn't help to go over 40 with the threshold, but it HURTS A LOT (exponential slowdown)
(def params
		 (atom {:size 20
						:elitism 1
						:immigration 1 
						:num-mutations 200
						:crossover 0.3
						:cross-fn ga/variable-crossover
						:gene-count-penalty-threshold 40}))

(def best-gamma-model ;THIS IS OUTDATED ... there is a newer gamma model.
		 {:fitness -1.8398655209003936,
			:genotype [6.437591133403866
								 2242.7850586250565
								 9.811875139475335
								 69.94044048571718
								 4.207150182519289
								 14.47167356064309
								-0.27062152752458823]})

;the top one is good.
;counts: (51 50 52 42 58 51 52 51)
;this seems to break our new search... the fitness values are so low.
(def initial-pool
		 [{:fitness -237.051596093788731, :genotype [{:type :impediment, :x 25, :width 12, :kind :hill1} {:type :enemy, :x 37, :kind 1, :num 1} {:type :hole, :x 45, :width 2} {:type :hole, :x 53, :width 1} {:type :hill, :x 56, :height 7, :width 4} {:type :enemy, :x 56, :kind 2, :num 1} {:type :blocks-with-enemies, :x 57, :width 4, :coins 0, :kind 2, :num 3} {:type :hill, :x 60, :height 10, :width 4} {:type :enemy, :x 65, :kind 0, :num 1} {:type :coin-row, :x 68, :width 4} {:type :enemy-pit-above, :x 72, :width 8, :kind 2, :num 3, :left :rock, :right :pipe} {:type :hole, :x 82, :width 4} {:type :hole, :x 88, :width 3} {:type :pipe, :x 93, :height 4, :piranha true} {:type :pipe, :x 97, :height 4, :piranha true} {:type :impediment, :x 100, :width 12, :kind :hill2} {:type :coin-arc, :x 112} {:type :coin-row, :x 116, :width 4} {:type :hole, :x 122, :width 3} {:type :pipe, :x 129, :height 2, :piranha false} {:size 1, :type :blocks, :x 132, :width 2, :coins 1} {:type :enemy, :x 134, :kind 1, :num 1} {:type :enemy, :x 135, :kind 2, :num 1} {:size 1, :type :blocks, :x 136, :width 1, :coins 1} {:type :enemy-pit, :x 139, :width 6, :kind 2, :num 1, :shell? false} {:size 1, :type :blocks, :x 146, :width 3, :coins 1} {:type :pipe, :x 150, :height 2, :piranha true} {:type :pipe, :x 154, :height 2, :piranha true} {:type :hill-with-enemies, :x 157, :height 7, :width 12, :kind 2, :num 2} {:type :hill-with-enemies, :x 169, :height 7, :width 12, :kind 2, :num 2} {:type :enemy, :x 181, :kind 0, :num 1} {:type :hill, :x 181, :height 8, :width 4} {:type :enemy, :x 182, :kind 0, :num 1} {:type :impediment, :x 183, :width 12, :kind :blocks} {:type :hill, :x 185, :height 8, :width 4} {:type :impediment, :x 195, :width 12, :kind :blocks} {:type :hole, :x 208, :width 2} {:type :hole, :x 212, :width 2} {:size 1, :type :blocks, :x 215, :width 2, :coins 1} {:size 1, :type :blocks, :x 217, :width 2, :coins 1} {:size 1, :type :blocks, :x 219, :width 1, :coins 1} {:size 1, :type :blocks, :x 220, :width 1, :coins 1} {:type :hole, :x 222, :width 3} {:type :hole, :x 227, :width 3} {:type :blocks, :x 231, :width 1, :coins 0} {:type :enemy, :x 232, :kind 0, :num 1} {:type :hole, :x 234, :width 3} {:type :enemy-row, :x 238, :kind 2, :num 4, :separation 3, :shell? true} {:size 1, :type :blocks, :x 251, :width 1, :coins 1} {:type :enemy-pit, :x 253, :width 4, :kind 3, :num 1, :shell? false} {:type :blocks-with-enemies, :x 258, :width 4, :coins 2, :kind 2, :num 2}]}
			{:fitness -244.095968973911587, :genotype [{:type :steps, :x 10, :height 1, :width 11, :reverse? false, :num 4} {:type :steps, :x 21, :height 1, :width 12, :reverse? true, :num 3} {:type :impediment, :x 34, :width 12, :kind :hill2} {:type :hole, :x 62, :width 3} {:type :hole, :x 67, :width 3} {:type :impediment, :x 71, :width 12, :kind :hill2} {:type :hill, :x 74, :height 9, :width 4} {:type :enemy-row, :x 83, :kind 2, :num 3, :separation 1, :shell? false} {:type :enemy-row, :x 87, :kind 2, :num 3, :separation 1, :shell? false} {:size 1, :type :blocks, :x 91, :width 2, :coins 1} {:type :hill-with-enemies, :x 93, :height 11, :width 4, :kind 2, :num 4} {:type :hill-with-enemies, :x 97, :height 9, :width 5, :kind 2, :num 3} {:type :coin-row, :x 102, :width 4} {:type :hole, :x 113, :width 4} {:type :enemy, :x 118, :kind 0, :num 1} {:size 1, :type :blocks, :x 119, :width 4, :coins 1} {:size 1, :type :blocks, :x 126, :width 1, :coins 1} {:type :hole, :x 146, :width 4} {:type :steps, :x 151, :height 1, :width 11, :reverse? false, :num 4} {:size 1, :type :blocks, :x 162, :width 2, :coins 1} {:size 1, :type :blocks, :x 164, :width 3, :coins 1} {:type :blocks, :x 167, :width 1, :coins 0} {:size 1, :type :blocks, :x 168, :width 1, :coins 1} {:type :coin-row, :x 169, :width 1} {:type :coin-arc, :x 170} {:type :hole, :x 171, :width 4} {:type :hill, :x 174, :height 10, :width 12} {:type :hole, :x 177, :width 1} {:type :enemy-row, :x 179, :kind 2, :num 3, :separation 2, :shell? false} {:type :hole, :x 181, :width 4} {:type :hole, :x 187, :width 2} {:type :blocks, :x 190, :width 1, :coins 0} {:type :hill-with-enemies, :x 191, :height 7, :width 9, :kind 0, :num 3} {:type :blocks, :x 200, :width 1, :coins 0} {:type :hill-with-enemies, :x 201, :height 9, :width 3, :kind 2, :num 4} {:type :hill-with-enemies, :x 204, :height 9, :width 3, :kind 2, :num 4} {:type :hill, :x 207, :height 11, :width 5} {:size 1, :type :blocks, :x 207, :width 1, :coins 1} {:size 1, :type :blocks, :x 208, :width 4, :coins 1} {:type :cannon, :x 219, :height 2} {:type :blocks-with-enemies, :x 221, :width 6, :coins 0, :kind 2, :num 2} {:type :blocks-with-enemies, :x 227, :width 6, :coins 1, :kind 1, :num 3} {:type :cannon, :x 234, :height 3} {:type :coin-row, :x 236, :width 5} {:type :hole, :x 238, :width 2} {:size 1, :type :blocks, :x 241, :width 1, :coins 1} {:type :coin-row, :x 242, :width 3} {:type :hole, :x 243, :width 1} {:type :impediment, :x 245, :width 12, :kind :pipe} {:type :blocks-with-enemies, :x 257, :width 6, :coins 0, :kind 2, :num 2}]}
			{:fitness -254.033210312634512, :genotype [{:type :coin-arc, :x 15} {:type :coin-arc, :x 19} {:type :hole, :x 35, :width 3} {:type :hole, :x 40, :width 3} {:type :hill-with-enemies, :x 44, :height 11, :width 10, :kind 2, :num 3} {:type :hill-with-enemies, :x 54, :height 11, :width 10, :kind 2, :num 3} {:type :enemy, :x 65, :kind 2, :num 1} {:type :enemy, :x 66, :kind 2, :num 1} {:type :enemy, :x 82, :kind 1, :num 1} {:type :hole, :x 91, :width 3} {:type :hole, :x 96, :width 4} {:type :hole, :x 102, :width 3} {:size 1, :type :blocks, :x 106, :width 4, :coins 1} {:type :hole, :x 111, :width 4} {:size 1, :type :blocks, :x 116, :width 4, :coins 1} {:size 1, :type :blocks, :x 120, :width 2, :coins 1} {:type :enemy, :x 122, :kind 0, :num 1} {:type :hole, :x 124, :width 4} {:type :hill, :x 128, :height 11, :width 5} {:type :hole, :x 136, :width 2} {:type :pipe, :x 140, :height 3, :piranha true} {:type :steps, :x 143, :height 3, :width 6, :reverse? true, :num 3} {:type :hill, :x 143, :height 10, :width 6} {:type :hill, :x 149, :height 9, :width 6} {:type :enemy, :x 149, :kind 0, :num 1} {:type :impediment, :x 150, :width 12, :kind :blocks} {:type :hill, :x 155, :height 10, :width 6} {:type :hill, :x 161, :height 7, :width 10} {:size 1, :type :blocks, :x 162, :width 4, :coins 1} {:type :impediment, :x 166, :width 12, :kind :hill2} {:type :hill, :x 171, :height 7, :width 8} {:type :blocks-with-enemies, :x 178, :width 5, :coins 0, :kind 0, :num 1} {:size 1, :type :blocks, :x 183, :width 1, :coins 1} {:size 1, :type :blocks, :x 184, :width 1, :coins 1} {:size 1, :type :blocks, :x 185, :width 3, :coins 1} {:size 1, :type :blocks, :x 188, :width 3, :coins 1} {:type :coin-arc, :x 191} {:type :enemy, :x 191, :kind 0, :num 1} {:size 1, :type :blocks, :x 195, :width 2, :coins 1} {:type :enemy-row, :x 197, :kind 0, :num 2, :separation 3, :shell? true} {:type :impediment, :x 198, :width 12, :kind :pipe} {:type :coin-arc, :x 210} {:type :hole, :x 211, :width 4} {:type :pipe, :x 217, :height 3, :piranha false} {:type :coin-row, :x 220, :width 6} {:type :hole, :x 221, :width 4} {:type :pipe, :x 227, :height 2, :piranha true} {:size 1, :type :blocks, :x 230, :width 4, :coins 1} {:type :pipe, :x 235, :height 2, :piranha true} {:type :steps, :x 238, :height 2, :width 8, :reverse? true, :num 2} {:type :coin-row, :x 238, :width 6} {:type :pipe, :x 247, :height 2, :piranha true}]}
			{:fitness -254.01287136648749, :genotype [{:type :enemy, :x 14, :kind 0, :num 1} {:size 1, :type :blocks, :x 23, :width 3, :coins 1} {:type :hole, :x 38, :width 2} {:type :hole, :x 42, :width 1} {:type :hill-with-enemies, :x 44, :height 11, :width 11, :kind 2, :num 3} {:type :hole, :x 56, :width 3} {:type :impediment, :x 60, :width 12, :kind :hill2} {:type :hole, :x 73, :width 3} {:type :blocks, :x 77, :width 1, :coins 0} {:type :coin-arc, :x 72} {:type :enemy-pit-above, :x 79, :width 10, :kind 2, :num 1, :left :rock, :right :rock} {:type :hill, :x 76, :height 9, :width 6} {:type :hole, :x 91, :width 4} {:size 1, :type :blocks, :x 96, :width 2, :coins 1} {:size 1, :type :blocks, :x 98, :width 1, :coins 1} {:type :hill, :x 82, :height 9, :width 6} {:type :enemy, :x 99, :kind 0, :num 1} {:type :enemy-pit, :x 101, :width 5, :kind 2, :num 2, :shell? false} {:type :enemy-pit, :x 102, :width 6, :kind 2, :num 2, :shell? false} {:type :coin-arc, :x 92} {:type :enemy, :x 109, :kind 1, :num 1} {:type :hole, :x 111, :width 2} {:type :cannon, :x 115, :height 1} {:type :cannon, :x 118, :height 1} {:type :steps, :x 120, :height 3, :width 10, :reverse? false, :num 1} {:type :coin-arc, :x 105} {:type :enemy, :x 130, :kind 1, :num 1} {:size 1, :type :blocks, :x 131, :width 3, :coins 1} {:type :cannon, :x 135, :height 1} {:type :hole, :x 138, :width 2} {:type :enemy-row, :x 141, :kind 2, :num 4, :separation 2, :shell? true} {:type :hole, :x 151, :width 4} {:type :pipe, :x 157, :height 3, :piranha false} {:size 1, :type :blocks, :x 160, :width 1, :coins 1} {:size 1, :type :blocks, :x 161, :width 4, :coins 1} {:type :hole, :x 166, :width 2} {:size 1, :type :blocks, :x 169, :width 1, :coins 1} {:size 1, :type :blocks, :x 170, :width 4, :coins 1} {:size 1, :type :blocks, :x 174, :width 4, :coins 1} {:type :hole, :x 197, :width 3} {:type :enemy-row, :x 210, :kind 1, :num 3, :separation 1, :shell? true} {:size 1, :type :blocks, :x 214, :width 1, :coins 1}]}
			{:fitness -287.060972452301394, :genotype [{:type :enemy-pit-above, :x 14, :width 12, :kind 2, :num 3, :left :rock, :right :cannon} {:type :hole, :x 28, :width 3} {:type :hole, :x 41, :width 3} {:type :coin-row, :x 41, :width 5} {:type :enemy-pit-above, :x 51, :width 12, :kind 2, :num 3, :left :rock, :right :cannon} {:type :pipe, :x 65, :height 4, :piranha false} {:type :hole, :x 69, :width 4} {:type :hole, :x 76, :width 4} {:type :pipe, :x 83, :height 4, :piranha false} {:type :coin-arc, :x 86} {:type :pipe, :x 91, :height 2, :piranha true} {:type :hole, :x 96, :width 2} {:size 1, :type :blocks, :x 99, :width 4, :coins 1} {:type :cannon, :x 104, :height 1} {:size 1, :type :blocks, :x 106, :width 1, :coins 1} {:type :coin-arc, :x 107} {:type :hole, :x 109, :width 2} {:type :coin-arc, :x 111} {:type :hill, :x 115, :height 10, :width 5} {:type :hole, :x 116, :width 2} {:type :enemy-row, :x 119, :kind 2, :num 4, :separation 2, :shell? false} {:type :hill, :x 120, :height 10, :width 5} {:type :hole, :x 129, :width 2} {:type :enemy, :x 132, :kind 0, :num 1} {:type :cannon, :x 134, :height 3} {:type :blocks, :x 136, :width 2, :coins 0} {:size 1, :type :blocks, :x 138, :width 3, :coins 1} {:type :enemy, :x 141, :kind 0, :num 1} {:type :hole, :x 143, :width 2} {:size 1, :type :blocks, :x 146, :width 2, :coins 1} {:type :steps, :x 148, :height 1, :width 15, :reverse? true, :num 3} {:type :blocks, :x 163, :width 1, :coins 0} {:type :enemy, :x 164, :kind 0, :num 1} {:type :pipe, :x 166, :height 4, :piranha false} {:type :pipe, :x 170, :height 4, :piranha false} {:type :cannon, :x 174, :height 1} {:type :hill, :x 176, :height 8, :width 11} {:size 1, :type :blocks, :x 176, :width 2, :coins 1} {:type :blocks, :x 178, :width 3, :coins 0} {:type :enemy, :x 181, :kind 0, :num 1} {:size 1, :type :blocks, :x 182, :width 2, :coins 1} {:type :hole, :x 185, :width 1} {:type :pipe, :x 188, :height 2, :piranha true} {:type :pipe, :x 192, :height 2, :piranha true} {:type :cannon, :x 196, :height 1} {:type :cannon, :x 199, :height 1} {:type :hill, :x 201, :height 8, :width 11} {:type :enemy-pit, :x 202, :width 5, :kind 3, :num 1, :shell? false} {:size 1, :type :blocks, :x 202, :width 3, :coins 1} {:type :enemy, :x 205, :kind 0, :num 1} {:size 1, :type :blocks, :x 206, :width 4, :coins 1} {:size 1, :type :blocks, :x 210, :width 1, :coins 1} {:type :cannon, :x 213, :height 1} {:type :enemy-pit, :x 216, :width 6, :kind 0, :num 1, :shell? true} {:type :impediment, :x 223, :width 12, :kind :pipe} {:size 1, :type :blocks, :x 235, :width 3, :coins 1} {:type :impediment, :x 238, :width 12, :kind :pipe} {:type :impediment, :x 250, :width 12, :kind :pipe}]}
			{:fitness -514.4830308761676, :genotype [{:type :coin-row, :x 8, :width 2} {:type :hill-with-enemies, :x 11, :height 7, :width 9, :kind 2, :num 2} {:type :enemy-pit-above, :x 23, :width 13, :kind 2, :num 1, :left :cannon, :right :pipe} {:type :hill, :x 22, :height 11, :width 10} {:type :impediment, :x 37, :width 12, :kind :blocks} {:type :hole, :x 55, :width 4} {:type :pipe, :x 67, :height 4, :piranha false} {:type :hole, :x 71, :width 4} {:type :enemy-row, :x 76, :kind 2, :num 4, :separation 2, :shell? false} {:type :hole, :x 90, :width 4} {:type :blocks-with-enemies, :x 101, :width 3, :coins 0, :kind 2, :num 3} {:type :impediment, :x 104, :width 12, :kind :hill2} {:type :hill, :x 117, :height 11, :width 10} {:type :hole, :x 119, :width 2} {:size 1, :type :blocks, :x 122, :width 2, :coins 1} {:type :hole, :x 126, :width 2} {:type :hole, :x 130, :width 4} {:type :coin-arc, :x 129} {:type :hill, :x 133, :height 10, :width 5} {:type :enemy-row, :x 135, :kind 2, :num 4, :separation 2, :shell? false} {:type :hill, :x 138, :height 10, :width 5} {:type :hole, :x 146, :width 4} {:type :enemy, :x 151, :kind 0, :num 1} {:size 1, :type :blocks, :x 152, :width 3, :coins 1} {:type :cannon, :x 156, :height 3} {:type :blocks, :x 158, :width 2, :coins 0} {:size 1, :type :blocks, :x 160, :width 3, :coins 1} {:size 1, :type :blocks, :x 163, :width 3, :coins 1} {:type :enemy, :x 166, :kind 0, :num 1} {:type :enemy, :x 167, :kind 2, :num 1} {:type :coin-arc, :x 167} {:type :hole, :x 169, :width 2} {:size 1, :type :blocks, :x 172, :width 3, :coins 1} {:type :enemy, :x 175, :kind 0, :num 1} {:size 1, :type :blocks, :x 176, :width 2, :coins 1} {:type :steps, :x 178, :height 1, :width 15, :reverse? true, :num 3} {:type :hole, :x 194, :width 4} {:type :enemy-pit, :x 200, :width 4, :kind 3, :num 1, :shell? true} {:type :enemy, :x 205, :kind 0, :num 1} {:type :blocks, :x 206, :width 1, :coins 0} {:type :enemy, :x 207, :kind 0, :num 1} {:type :enemy, :x 208, :kind 1, :num 1} {:type :pipe, :x 210, :height 4, :piranha false} {:size 1, :type :blocks, :x 213, :width 2, :coins 1} {:size 1, :type :blocks, :x 215, :width 4, :coins 1} {:size 1, :type :blocks, :x 219, :width 2, :coins 1} {:type :blocks-with-enemies, :x 235, :width 6, :coins 0, :kind 2, :num 2} {:type :impediment, :x 244, :width 12, :kind :pipe} {:size 1, :type :blocks, :x 256, :width 1, :coins 1} {:type :coin-row, :x 257, :width 3} {:type :hole, :x 260, :width 1}]}
			{:fitness -374.086092286170475, :genotype [{:type :enemy-row, :x 18, :kind 2, :num 3, :separation 3, :shell? true} {:type :enemy-row, :x 28, :kind 2, :num 3, :separation 3, :shell? true} {:type :enemy-row, :x 42, :kind 2, :num 3, :separation 3, :shell? true} {:type :hole, :x 58, :width 3} {:type :steps, :x 62, :height 1, :width 9, :reverse? true, :num 2} {:type :enemy-row, :x 71, :kind 2, :num 3, :separation 3, :shell? true} {:type :cannon, :x 86, :height 3} {:type :enemy, :x 88, :kind 0, :num 1} {:type :hole, :x 91, :width 4} {:size 1, :type :blocks, :x 96, :width 1, :coins 1} {:size 1, :type :blocks, :x 97, :width 1, :coins 1} {:type :hole, :x 99, :width 4} {:type :enemy-row, :x 104, :kind 2, :num 4, :separation 1, :shell? true} {:type :hole, :x 110, :width 4} {:type :cannon, :x 116, :height 3} {:size 1, :type :blocks, :x 118, :width 1, :coins 1} {:type :hill, :x 118, :height 8, :width 10} {:size 1, :type :blocks, :x 119, :width 1, :coins 1} {:type :steps, :x 120, :height 1, :width 9, :reverse? true, :num 2} {:type :hill, :x 128, :height 8, :width 10} {:type :enemy, :x 129, :kind 0, :num 1} {:type :enemy, :x 130, :kind 1, :num 1} {:type :enemy-row, :x 131, :kind 2, :num 3, :separation 3, :shell? true} {:type :hill, :x 138, :height 8, :width 10} {:type :blocks-with-enemies, :x 141, :width 3, :coins 0, :kind 2, :num 1} {:type :blocks-with-enemies, :x 144, :width 3, :coins 0, :kind 2, :num 1} {:type :enemy, :x 147, :kind 1, :num 1} {:type :hole, :x 149, :width 2} {:type :blocks-with-enemies, :x 152, :width 3, :coins 0, :kind 2, :num 1} {:size 1, :type :blocks, :x 155, :width 3, :coins 1} {:size 1, :type :blocks, :x 158, :width 3, :coins 1} {:type :enemy, :x 161, :kind 1, :num 1} {:type :enemy, :x 162, :kind 1, :num 1} {:size 1, :type :blocks, :x 163, :width 1, :coins 1} {:size 1, :type :blocks, :x 164, :width 1, :coins 1} {:type :hole, :x 166, :width 2} {:type :hole, :x 170, :width 2} {:type :hole, :x 174, :width 2} {:type :enemy, :x 177, :kind 1, :num 1} {:size 1, :type :blocks, :x 178, :width 4, :coins 1} {:size 1, :type :blocks, :x 182, :width 4, :coins 1} {:type :enemy-row, :x 186, :kind 2, :num 3, :separation 3, :shell? false} {:type :enemy-row, :x 196, :kind 2, :num 3, :separation 3, :shell? true} {:type :hole, :x 207, :width 3} {:type :enemy-row, :x 211, :kind 2, :num 3, :separation 3, :shell? true} {:type :enemy-row, :x 221, :kind 2, :num 3, :separation 3, :shell? true} {:type :enemy-row, :x 231, :kind 2, :num 3, :separation 3, :shell? true} {:type :coin-arc, :x 237} {:type :hole, :x 242, :width 3} {:type :coin-arc, :x 241} {:type :enemy-row, :x 246, :kind 2, :num 3, :separation 3, :shell? true} {:type :enemy-row, :x 256, :kind 2, :num 3, :separation 3, :shell? true}]}
			{:fitness -316.05987492152693, :genotype [{:type :hole, :x 17, :width 2} {:type :coin-arc, :x 18} {:type :coin-row, :x 22, :width 3} {:type :enemy-pit, :x 29, :width 5, :kind 2, :num 1, :shell? false} {:type :hole, :x 30, :width 3} {:type :hole, :x 35, :width 3} {:size 1, :type :blocks, :x 39, :width 4, :coins 1} {:size 1, :type :blocks, :x 43, :width 4, :coins 1} {:type :enemy, :x 47, :kind 1, :num 1} {:type :pipe, :x 49, :height 3, :piranha false} {:type :pipe, :x 53, :height 3, :piranha false} {:type :pipe, :x 57, :height 3, :piranha false} {:size 1, :type :blocks, :x 60, :width 4, :coins 1} {:type :enemy-pit, :x 65, :width 6, :kind 2, :num 3, :shell? false} {:type :hill, :x 60, :height 7, :width 10} {:type :hill, :x 38, :height 7, :width 10} {:type :enemy-pit, :x 73, :width 6, :kind 2, :num 3, :shell? false} {:type :hill, :x 70, :height 7, :width 10} {:type :enemy-pit, :x 81, :width 6, :kind 2, :num 3, :shell? false} {:size 1, :type :blocks, :x 88, :width 3, :coins 1} {:type :hill, :x 80, :height 7, :width 10} {:type :hole, :x 92, :width 3} {:type :coin-arc, :x 91} {:type :coin-arc, :x 95} {:type :coin-arc, :x 99} {:size 1, :type :blocks, :x 103, :width 1, :coins 1} {:type :hill-with-enemies, :x 104, :height 10, :width 11, :kind 0, :num 1} {:type :enemy, :x 119, :kind 0, :num 1} {:type :impediment, :x 121, :width 12, :kind :blocks} {:type :impediment, :x 133, :width 12, :kind :blocks} {:size 1, :type :blocks, :x 145, :width 1, :coins 1} {:type :hole, :x 147, :width 3} {:type :hill-with-enemies, :x 151, :height 7, :width 6, :kind 1, :num 1} {:type :hole, :x 158, :width 3} {:type :hole, :x 163, :width 3} {:type :enemy, :x 167, :kind 2, :num 1} {:size 1, :type :blocks, :x 168, :width 2, :coins 1} {:type :coin-row, :x 170, :width 3} {:type :coin-row, :x 173, :width 3} {:type :hole, :x 174, :width 4} {:type :hole, :x 180, :width 4} {:size 1, :type :blocks, :x 185, :width 4, :coins 1} {:size 1, :type :blocks, :x 189, :width 1, :coins 1} {:size 1, :type :blocks, :x 190, :width 1, :coins 1} {:type :hole, :x 194, :width 3} {:type :hill-with-enemies, :x 198, :height 10, :width 10, :kind 2, :num 3} {:type :hill-with-enemies, :x 208, :height 10, :width 10, :kind 2, :num 3} {:type :hill-with-enemies, :x 218, :height 10, :width 10, :kind 2, :num 3} {:type :hill-with-enemies, :x 228, :height 10, :width 10, :kind 2, :num 3} {:type :enemy-pit, :x 243, :width 6, :kind 0, :num 3, :shell? true} {:type :blocks, :x 250, :width 4, :coins 0}]}])

;HACKEY REBINDING TO REMOVE THOSE FITNESS VALUES
(def initial-pool (vec (map #(assoc % :fitness 999) initial-pool)))

(def current-best-indiv (atom (first (shuffle initial-pool))))

(def current-best-generated
		 (atom (lg/generate-level (:genotype @current-best-indiv)
															Constraints/levelWidth)))

;30 dies 2/3rds of the time now now that it's harder evolving...

(defn rand-level [size]
	(vec (sort-by :x (take size (repeatedly fe/rand-gene)))))

(defn rand-indiv []
	{:genotype (rand-level (+ 5 (rand-int 15)))})

;-------------Population initialization--------------------------------
(swap! fe/level-length (constantly (- Constraints/levelWidth 60)))
(def generation (ref 0))
(def pop
	(let [seed (take (int (/ (:size @params) 2)) (shuffle initial-pool))
				random-guys (take (- (:size @params) (count seed)) (repeatedly rand-indiv))]
		(ref
		 (vec (concat seed random-guys)))))

(def mutate-scale (atom 1.0))
;(swap! mutate-scale #(* % 0.1))
;----------------------------------------------------------------------

(defn mutate-indiv [{:keys [genotype] :as indiv}]
	(condp > (rand)
		0.2 (update-in indiv [:genotype] (comp vec conj) (fe/rand-gene)) ;this vec shouldn't be necessary, but just in case genotype is nil, we need to make sure we conj into a Vector, not a List.
		0.4 (let [del (rand-int (count genotype))]
					(assoc indiv :genotype (vec (concat
																			 (take del genotype)
																			 (drop (inc del) genotype)))))
		1.0 (if (> (count genotype) 0)
					(let [pick (rand-int (count genotype))]
						(update-in indiv [:genotype pick]
											 (condp > (rand)
												 0.5 (fn [{old-x :x}] (assoc (fe/rand-gene) :x old-x))
												 1.0 (fn [gene]
															 (update-in gene [:x]
																					#(clamp (+ % (rand-gauss 10.0))
																						0 260))))))
					indiv)))

;CAUTION!!! THIS IS A FAKE HACK to test something out
#_(defn mutate-indiv [{:keys [genotype] :as indiv}]
	(condp > (rand)
		0.25 (update-in indiv [:genotype] conj (fe/rand-gene))
		0.5 (let [del (rand-int (count genotype))]
					(assoc indiv :genotype (vec (concat
																			 (take del genotype)
																			 (drop (inc del) genotype)))))
		1.0 (update-in indiv [:genotype (rand-int (count genotype)) :x]
									 (fn [x] (clamp (+ x (rand-gauss 10)) 0 260)))))

(defn gene-to-gamma
	"converts a gene to something the gamma model can use,
   approximate difficulty differences through mulitplicity"
	[gene]
	(letfn [(mult
						([acc] (mult acc 1))
						([acc spread] (map-indexed
													 (fn [i g] (update-in g [:x] (fn [x] (+ x (* spread i)))))
													 (repeat (acc gene) gene))))]
		(case (:type gene)
					:blocks []
					:coin-arc []
					:coin-row []
					:hill []
					:steps []
					:hole (mult :width)
					:enemy-pit (mult :num)
					:enemy-pit-above (mult :num)
					:blocks-with-enemies (mult :num)
					:enemy-row (mult :num (:separation gene))
					:hill-with-enemies (mult :num)
					[gene])))

(defn indiv-to-gamma [indiv]
	(sort-by :x (map (fn [g] (assoc g :difficulty 1))
									 (mapcat gene-to-gamma
													 (:genotype indiv)))))

;CAUTION!!! this is a fake hack to just test out the evolution!
#_(defn indiv-to-gamma [indiv]
	(map (fn [g] (assoc g :difficulty 1))
			 (sort-by :x (:genotype indiv))))

(defn distribution
	[genotype]
	(reduce #(update-in %1 [%2]
											(fn [x] (if (nil? x) 1 (inc x))))
					{}
					(map :type genotype)))

(def challenge-scale 10.0)
(defn fitness-indiv
	"alternate to leval/fitness-fun I guess... uses the 'real' gamma model...
   but I re-worked the old one to use the real model too... the even newer one from
   the journal."
	[indiv]
	(+ (gm/gamma-level-fitness best-gamma-model
													(mapcat gene-to-gamma (:genotype indiv))
													challenge-scale)
		 (- (* 10.0 (reduce + (map sqrt (vals (distribution (:genotype indiv)))))))))

(defn repair-indiv [indiv]
	(update-in indiv [:genotype] leval/timed-constrain))

(defn repair-pop
	"bakes solved constraints into genes, deletes unfixable levels"
	[pop]
	(vec
	 (concat
		(take (:elitism @params) pop)
		(filter #(not (nil? %))
						(map
						 repair-indiv
						 (drop (:elitism @params) pop))))))

(defn count-turtles [genotype]
	(reduce (fn [acc
							 gene]
						(+ acc
							 (if (#{:enemy :enemy-pit :enemy-pit-above
											:hill-with-enemies :enemy-row
											:blocks-with-enemies}
										(:type gene))
								 (if (#{0 1 3} (:kind gene))
									 (:num gene)
									 (if (and (= :enemy-row (:kind gene))
														(= true (:shell? gene)))
										 1
										 0))							
								 0)))
					0
					genotype))

(defn remove-turtles
	[genotype num]
	(first
	 (reduce (fn [[newgt,cnt] gene]
						 (if (and
									(#{:enemy-row} (:type gene)) ;enemy pit dosn't use shell.
									(true? (:shell? gene))
									(not (#{0 1 3} (:kind gene))))
							 (if (pos? cnt)
								 [(conj newgt (merge gene {:shell? false}))
									(dec cnt)]
								 [(conj newgt gene), cnt])
							 (if (and
										(#{:enemy
												 :enemy-pit
												 :enemy-pit-above
												 :blocks-with-enemies
												 :enemy-row
												 :hill-with-enemies} (:type gene))
											(#{0 1 3} (:kind gene)))
									 (let [n (:num gene)]
										 (if (>= cnt n)
											 [newgt, (- cnt n)]
											 [(conj newgt (merge gene {:num (- n cnt)})), 0]))
									 [(conj newgt gene), cnt])))
					 [[],num]
					 genotype)))

(defn add-turtles [genotype num]
	(vec (sort-by
				:x
				(concat genotype (repeatedly num
																		 #(merge (fe/rand-enemy)
																						 {:kind (rand-elt [0 1])}))))))

(defn count-coins [genotype]
	(reduce (fn [acc g]
						(+ acc
							 (if (#{:blocks :blocks-with-enemies} (:type g))
								 (:coins g)
								 0)))
					0
					genotype))

(defn add-coins [genotype num]
	(vec (sort-by
				:x
				(concat genotype (repeatedly num
																		 #(merge (fe/rand-blocks)
																						 {:coins 1 :size 1}))))))
(defn remove-coins-at-least
	"may remove too many coin blocks.
   returns a vector pair of the genotype and a
   possibly negative value of how many extra
   coins in debt you are."
	[genotype num]
	(reduce (fn [[newgt, cnt] gene]
						(let [cs (:coins gene)]
							(if (nil? cs)
								[(conj newgt gene),cnt]
								(if (and (pos? cnt) (pos? cs))
									[newgt,(- cnt cs)]
									[(conj newgt gene), cnt]))))
					[[], num]
					genotype))

(defn count-holes [genotype]
	(count (filter #(= :hole (:type %)) genotype)))

(defn remove-holes [genotype num]
	"remove num holes at random."
	(let [h (count-holes genotype)
				kill (into #{} (take num (shuffle (range h))))]
		(first
		(reduce
		 (fn [[newg cnt] gene]
			 (if (= :hole (:type gene))
				 [(if (kill cnt)
						newg
						(conj newg gene))
					, (inc cnt)]
				 [(conj newg gene), cnt]))
		 [[],0]
		 genotype))))

(defn remove-all-holes [genotype]
	(vec (filter (fn [g] (not (= (:type g) :hole))) genotype)))

(defn add-holes [genotype num]
	(vec (sort-by
				:x
				(concat genotype (repeatedly num fe/rand-hole)))))

(defn set-competition-constraints
	"ensures a level satisfies the competition level constraints"
	[genotype]
	(let [ch (count-holes genotype)
				hole-diff (- ch Constraints/gaps)
				h-fixed (if (< hole-diff 0)
									(add-holes genotype (- hole-diff))
									(remove-holes genotype hole-diff))
				ct (count-turtles genotype)
				t-diff (- ct Constraints/turtels)
				t-fixed (if (< t-diff 0)
									(add-turtles h-fixed (- t-diff))
									(remove-turtles h-fixed t-diff))
				cc (count-coins t-fixed)
				c-diff (- cc Constraints/coinBlocks)
				c-fixed (if (> c-diff 0)
									(let [[c1, bal] (remove-coins-at-least t-fixed c-diff)]
										(add-coins c1 (- bal)))
									(add-coins t-fixed (- c-diff)))]
		c-fixed))

(defn fix-competition-constraints
	"repairs a population to obey competition constraints
   CAUTION: swapped out the monolithi set-competition-constraints
   for the more specific remove-all-holes"
	[pop]
	(map #(update-in % [:genotype] remove-all-holes) pop))
;-----------------------------------------------------------------------
;------------------------------ the GA --------------------------------
;-----------------------------------------------------------------------

;timing
(defn time-now []
	(let [cal (Calendar/getInstance)
				sdf (SimpleDateFormat. "H:mm:ss:SSS")]
		#_(.format sdf (.getTime cal))
		(.getTimeInMillis cal)))

(def start-time (atom (time-now)))
(defn reset-start [] (swap! start-time (fn [_] (time-now))))
(defn time-relative []
	(int (/ (- (time-now) @start-time) 1000)))

(swap! params merge
			 {:mutate-fn mutate-indiv
				:rand-indiv rand-indiv})

(def fitplot (atom [[-1 99999 (time-relative)]]))

(defn step []
	"a regular evolutionary step with 1 population.
  relies on the next-generation fn to fix genotypes"
	(dosync
	 (commute pop #(-> %
										 (ga/next-generation @params)
										 (fix-competition-constraints) ;TURNED OFF COMPETITION REQUIREMENTS
										 (repair-pop)
										 (ga/sort-generation (fn [indiv] 
																					 (+ (leval/fitness-fun (indiv-to-gamma indiv))
																							(max (:gene-count-penalty-threshold @params)
																									 (count (:genotype indiv)))))))) ;fitness-indiv
	 (commute generation inc)
	 (swap! current-best-indiv
					(fn [{:keys [fitness] :as indiv}]
						(if (< (:fitness (first @pop))
									 fitness)
							(first @pop)
							indiv)))
	 (swap! fitplot (fn [dats]
										(let [cur-fitness (:fitness (first @pop))
													[gen fit time] (last dats)]
											(if (< cur-fitness
														 fit)
												(conj dats [@generation cur-fitness (time-relative)])
												dats)))))
	[@generation (:fitness (first @pop))])

;make this less repetitive (throw in some macros)

(def *runthread* (atom nil))

(defn stop []
  (. @*runthread* stop))

(defn start []
	(do
		(reset-start)
		(swap! *runthread* (fn [_] (ga/makethread step)))
		(. @*runthread* start)))

(defn reboot []
	(dosync
	 (ref-set generation 0)
	 (ref-set pop (vec (take (:size @params) (repeatedly rand-indiv))))
	 (swap! mutate-scale (constantly 1.0))))

(defn run-best []
	(let [lev (lg/generate-level (:genotype (first @pop)) Constraints/levelWidth)]
		(PlayCustomized/runLevel lev)))

(defn run-absolute-best []
	(let [lev (lg/generate-level (:genotype @current-best-indiv) Constraints/levelWidth)]
		(PlayCustomized/runLevel lev)))

(defn run-indiv [indiv]
	(let [lev (lg/generate-level (:genotype indiv) Constraints/levelWidth)]
		(PlayCustomized/runLevel lev)))

(defn run-level [level]
	(PlayCustomized/runLevel level))

(defn view-history-indiv
	"this is for the 'pure' statehistory with no real measuing of difficulty etc."
	[indiv]
	(leval/gamma-statehistory-mario
	 (map (fn [g] (assoc g :difficulty 1))
				(indiv-to-gamma indiv))))

;(prn @generation (float @mutate-scale) (update-in (first @pop) [:fitness] #(/ % 1000000)))
;(map :anxiety (gamma-statehistory (first @pop) (first *levels*))) ;show a run of a level
;(map #(count (filter #{0} (map :anxiety (gamma-statehistory (first @gamma-pop) %)))) *negative-levels*) ;count the number of "discharges"
;(map :set (gamma-fitness-sorted (first @gamma-pop))) ;for charting
;(map #(apply max (separation (map :x %))) *levels*) ;for separation
;(map #(gamma-level-fitness (first @gamma-pop) %) *levels*) ;for seeing fitness values

;how to view the history of an individuals
;(leval/gamma-statehistory-mario (map (fn [g] (assoc g :difficulty 1)) (mapcat gene-to-gamma (:genotype (first @pop)))))

;how to view a scatterplot (with some y jitter to "spread out" the datapoints)
;(view (incanter.charts/scatter-plot (map :x (:genotype (first @pop))) (repeatedly 1000 #(rand-gauss 1))))

;how to view a varying model
;(view (incanter.charts/xy-plot (range 280) (for [x (range 280)] (second (leval/varying-gmodel x)))))


;This chart is the varying M compared to the actual anxiety-per-rhythm group
 ;300

(defn chart
	"return the chart object"
	[indiv]
	(-> (incanter.charts/xy-plot (range 0 251 10)
															 (for [x (range 0 251 10)] (second (leval/varying-gmodel x)))
															 :series-label "M"
															 :x-label  "t"
															 :y-label "Anxiety"
															 :legend true)
			(incanter.charts/set-stroke :width 2 :dash 5)
			(incanter.charts/add-lines
			 (sort (map #(clamp (+  % (rand-gauss 1)) 0 260)
									(concat [0] (map :x (indiv-to-gamma indiv)))))
			 (map :anxiety (view-history-indiv indiv))
			 :series-label "Anxiety")
			(gm/set-nate-theme)
			))

(defn show [indiv]
	(view (chart indiv)
				:width 370
				:height 200))

(defn save-pdf-anx [indiv]
	(save-pdf (chart indiv)
						"charts/anx.pdf"
						:width 370
						:height 200))

;the anxiety graph for a certian indiv.. it's kinda broken
#_(show (chart {:fitness 185.05746346674184, :genotype [{:type :cannon, :x 12, :height 1} {:type :enemy, :x 23, :kind 0, :num 1} {:type :enemy, :x 23, :kind 0, :num 1} {:type :enemy, :x 34, :kind 1, :num 1} {:type :enemy, :x 34, :kind 1, :num 1} {:type :enemy-pit, :x 47, :width 5, :kind 2, :num 3, :shell? false} {:type :hole, :x 65, :width 2} {:type :hole, :x 106, :width 4} {:type :hole, :x 93, :width 1} {:type :hole, :x 106, :width 4} {:type :blocks, :x 117, :width 1, :coins 1} {:type :enemy-pit-above, :x 79, :width 12, :kind 3, :num 3, :left :cannon, :right :cannon} {:type :blocks, :x 117, :width 1, :coins 1} {:type :blocks, :x 117, :width 1, :coins 1} {:type :enemy, :x 34, :kind 1, :num 1} {:type :enemy-pit, :x 47, :width 5, :kind 2, :num 3, :shell? false} {:type :hole, :x 65, :width 2} {:type :blocks, :x 90, :width 1, :coins 0} {:type :hole, :x 93, :width 1} {:type :blocks, :x 90, :width 1, :coins 0} {:type :hole, :x 93, :width 1} {:type :enemy-pit-above, :x 79, :width 12, :kind 3, :num 3, :left :cannon, :right :cannon} {:type :hole, :x 106, :width 4} {:type :blocks, :x 117, :width 1, :coins 1} {:type :blocks, :x 117, :width 1, :coins 1} {:type :enemy-pit-above, :x 79, :width 12, :kind 3, :num 3, :left :cannon, :right :cannon} {:type :hole, :x 106, :width 4} {:type :cannon, :x 12, :height 1} {:type :enemy, :x 23, :kind 0, :num 1} {:type :enemy, :x 34, :kind 1, :num 1} {:type :enemy-pit, :x 47, :width 5, :kind 2, :num 3, :shell? false} {:type :hole, :x 65, :width 2} {:type :enemy-pit, :x 47, :width 5, :kind 2, :num 3, :shell? false} {:type :hole, :x 65, :width 2} {:type :hole, :x 93, :width 1} {:type :hole, :x 106, :width 4} {:type :hole, :x 65, :width 2} {:type :hole, :x 65, :width 2} {:type :hole, :x 93, :width 1} {:type :hole, :x 106, :width 4} {:type :hole, :x 93, :width 1} {:type :hole, :x 106, :width 4} {:type :blocks, :x 117, :width 1, :coins 1} {:type :enemy-pit-above, :x 79, :width 12, :kind 3, :num 3, :left :cannon, :right :cannon} {:type :blocks, :x 117, :width 1, :coins 1} {:type :blocks, :x 117, :width 1, :coins 1} {:type :enemy-pit-above, :x 79, :width 12, :kind 3, :num 3, :left :cannon, :right :cannon} {:type :enemy-pit-above, :x 79, :width 12, :kind 3, :num 3, :left :cannon, :right :cannon} {:type :enemy-pit-above, :x 79, :width 12, :kind 3, :num 3, :left :cannon, :right :cannon} {:type :hole, :x 106, :width 4} {:size 1, :type :blocks, :x 182, :width 4, :coins 1} {:type :blocks, :x 117, :width 1, :coins 1} {:type :blocks, :x 130, :width 2, :coins 0} {:type :enemy, :x 53, :kind 1, :num 1} {:size 1, :type :blocks, :x 182, :width 4, :coins 1} {:type :blocks, :x 117, :width 1, :coins 1} {:type :blocks, :x 130, :width 2, :coins 0} {:type :cannon, :x 159, :height 3} {:type :blocks, :x 130, :width 2, :coins 0} {:type :cannon, :x 159, :height 3} {:type :blocks, :x 130, :width 2, :coins 0} {:type :blocks, :x 130, :width 2, :coins 0} {:type :hole, :x 106, :width 4} {:type :blocks, :x 130, :width 2, :coins 0} {:type :cannon, :x 159, :height 3} {:type :blocks, :x 130, :width 2, :coins 0} {:type :cannon, :x 159, :height 3} {:type :blocks, :x 130, :width 2, :coins 0} {:type :cannon, :x 159, :height 3} {:type :blocks, :x 130, :width 2, :coins 0} {:type :blocks, :x 165, :width 2, :coins 1} {:type :hole, :x 141, :width 1} {:type :hill, :x 142, :height 7, :width 8} {:type :blocks-with-enemies, :x 145, :width 5, :coins 1, :kind 0, :num 1} {:type :enemy-row, :x 196, :kind 2, :num 3, :separation 3, :shell? true} {:type :blocks-with-enemies, :x 145, :width 5, :coins 1, :kind 0, :num 1} {:type :hole, :x 141, :width 1} {:type :enemy-row, :x 196, :kind 2, :num 3, :separation 3, :shell? true} {:type :blocks-with-enemies, :x 145, :width 5, :coins 1, :kind 0, :num 1} {:type :blocks-with-enemies, :x 145, :width 5, :coins 1, :kind 0, :num 1} {:size 1, :type :blocks, :x 182, :width 4, :coins 1} {:type :blocks-with-enemies, :x 145, :width 5, :coins 1, :kind 0, :num 1} {:size 1, :type :blocks, :x 182, :width 4, :coins 1} {:type :enemy-row, :x 196, :kind 2, :num 3, :separation 3, :shell? true} {:size 1, :type :blocks, :x 182, :width 4, :coins 1} {:type :enemy-row, :x 196, :kind 2, :num 3, :separation 3, :shell? true} {:type :hole, :x 207, :width 3} {:type :enemy-row, :x 246, :kind 2, :num 3, :separation 3, :shell? true} {:type :enemy-row, :x 256, :kind 2, :num 3, :separation 3, :shell? true}]}))

;;-----------TODO: move this offline rendering stuff somewhere nicer.-----

(def my-indiv {:fitness 30.836500510093437, :genotype [{:type :enemy-pit-above, :x 92, :width 10, :kind 3, :num 3, :left :rock, :right :cannon} {:type :hole, :x 104, :width 3} {:type :pipe, :x 209, :height 3, :piranha true} {:type :steps, :x 139, :height 3, :width 6, :reverse? true, :num 3} {:type :hill, :x 139, :height 10, :width 6} {:type :enemy-pit-above, :x 80, :width 10, :kind 3, :num 3, :left :rock, :right :cannon} {:type :hole, :x 109, :width 3} {:type :pipe, :x 201, :height 3, :piranha true} {:type :steps, :x 133, :height 3, :width 6, :reverse? true, :num 3} {:type :hill, :x 145, :height 10, :width 6} {:type :hill, :x 151, :height 9, :width 6} {:type :enemy, :x 148, :kind 0, :num 1} {:type :impediment, :x 162, :width 12, :kind :blocks} {:type :pipe, :x 114, :height 3, :piranha false} {:type :pipe, :x 197, :height 3, :piranha true} {:type :impediment, :x 162, :width 12, :kind :blocks} {:type :pipe, :x 115, :height 3, :piranha false} {:type :pipe, :x 196, :height 3, :piranha true} {:type :steps, :x 127, :height 3, :width 6, :reverse? true, :num 3} {:type :hill, :x 133, :height 10, :width 6} {:type :hill, :x 157, :height 9, :width 6} {:type :enemy, :x 147, :kind 0, :num 1} {:type :impediment, :x 150, :width 12, :kind :blocks} {:type :pipe, :x 118, :height 3, :piranha false} {:type :pipe, :x 122, :height 3, :piranha false} {:type :enemy-pit, :x 252, :width 5, :kind 3, :num 1, :shell? false} {:type :enemy-pit-above, :x 54, :width 10, :kind 3, :num 3, :left :rock, :right :cannon} {:type :hole, :x 69, :width 3} {:type :pipe, :x 192, :height 3, :piranha true} {:type :steps, :x 70, :height 3, :width 6, :reverse? true, :num 3} {:type :hill, :x 127, :height 10, :width 6} {:type :hill, :x 163, :height 9, :width 6} {:type :enemy, :x 146, :kind 0, :num 1} {:type :impediment, :x 208, :width 12, :kind :blocks} {:type :steps, :x 71, :height 3, :width 6, :reverse? true, :num 3} {:type :enemy-pit-above, :x 42, :width 10, :kind 3, :num 3, :left :rock, :right :cannon} {:size 1, :type :blocks, :x 226, :width 3, :coins 1} {:type :impediment, :x 229, :width 12, :kind :pipe} {:type :impediment, :x 22, :width 12, :kind :pipe}]}
)

; (* 16 260)  = 4160 ... 320x16=5120
;(* 15 16) = 240
;need a Graphics2d in the g (.getGraphics image),
;need a width, height, a level, an Image (image)

(defn init-art-config
  "Call before rendering to load gfx"
  []
  (def graphics-environment (GraphicsEnvironment/getLocalGraphicsEnvironment))
  (def graphics-device (.getDefaultScreenDevice graphics-environment))
  (def graphics-configuration (.getDefaultConfiguration graphics-device))
  (Art/init graphics-configuration nil))

(defn draw-level-indiv
	"saves two pngs, I couldn't figure out how to composit them in Java at this time"
	[indiv filename-root]
	(let [level-renderer (LevelRenderer.)
				image (BufferedImage. 4384 256 BufferedImage/TYPE_INT_ARGB_PRE)
				graphics (.getGraphics image)
				;_ (.setComposite graphics (java.awt.AlphaComposite/Xor))

				_ (set! (. level-renderer image) image)
				_ (set! (. level-renderer g) graphics)
				_ (set! (. level-renderer width) 4384)
				_ (set! (. level-renderer height) 240)
				_ (.setCam level-renderer 0 0)
			  the-level (lg/generate-level (:genotype indiv) Constraints/levelWidth)
				_ (.setLevel level-renderer the-level)

				_ (.render level-renderer graphics 0 1.0)

				image2 (BufferedImage. 4384 256 BufferedImage/TYPE_INT_ARGB_PRE)
				graphics2 (.getGraphics image2)
				
				_ (doall (for [x (range 320) y (range 15)]
									 (when-let [template (aget (. the-level spriteTemplates) x y)]
										 (let [sprite (.spawn template x y)
													 offset (if (= 4 #_flower (.type template)) -40 0)
													 _ (set! (. sprite y) (+ (. sprite y) offset))]
											 (.render sprite graphics2 1.0)))))
				_ (.renderExit0 level-renderer graphics2 0 1.0 true)
				_ (.renderExit1 level-renderer graphics2 0 1.0)

				_ (im/save-image image (str "level_saves/" filename-root "-level.png")) ;before clobbering

				_ (.setComposite graphics (AlphaComposite/getInstance  AlphaComposite/SRC_OVER))
				_ (.setComposite graphics2 (AlphaComposite/getInstance  AlphaComposite/SRC_OVER))
				_ (.drawImage graphics image2 0 0 nil)]

		;AlphaComposite.getInstance(AlphaComposite.SRC)
		(do
			(im/save-image image (str "level_saves/" filename-root ".png"))
			(im/save-image image2 (str "level_saves/" filename-root "-enemies.png")))))
	
#_(def the-level (lg/generate-level (:genotype my-indiv) Constraints/levelWidth))

#_(map #(.type %) (filter identity (doall (for [x (range 320) y (range 15)]
													(when-let [template (aget (. the-level spriteTemplates) x y)]
														(.spawn template x y))))))

; 320x15

;we need to create the Enemy's manually, from the SpriteTemplates.
	;(.setBackground graphics (Color. 156 226 228))
;(.clearRect graphics 0 0 4384 256)


;--------how to test "feasibility" in the mario sense.
;tricky because the JaCoP system adjusts x coordinates by 1 sometimes for no reason.
(defn close? [gene1 gene2]
	(<= (Math/abs (- (:x gene1) (:x gene2))) 1))

(defn soft-feasible? [indiv1 indiv2]
	(nil? (get (frequencies (map close? (:genotype indiv1) (:genotype indiv2)))
						 false)))

;How to measure the liklihood of generating a feasible level by accident
;(frequencies (repeatedly 100 #(let [i (rand-indiv)] (soft-feasible? i (repair-indiv i)))))

;---------My sample level that demonstrates a bunch of DEs.-----------------
(def demonstrator {:genotype [{:type :blocks :x 10 :width 4 :coins 0}
															{:type :hill :x 20 :height 8 :width 4}
															{:type :hill-with-enemies :x 30 :height 7 :width 5 :kind 0 :num 3}
															{:type :cannon :x 40 :height 3}
															{:type :steps :x 50 :height 4 :width 8 :num 2}
															{:type :enemy-pit :x 60 :width 5 :kind 1 :num 3 :shell false}
															{:type :enemy-pit-above :x 70 :width 5 :kind 2 :num 3 :shell false :left :rock :right :pipe}
															{:type :enemy-row :x 80 :kind 0 :num 4 :separation 1 :shell? true}
															{:type :coin-arc :x 90}
															{:type :impediment :x 100 :width 12 :kind :pipe}
															]})

#_(draw-level-indiv demonstrator "complex-des")


#_(leval/gamma-statehistory-mario (indiv-to-gamma indiv))
;
;to get the x,y coordinates used to chart:
;(def my-coords (map vector (concat [0] (map :x (indiv-to-gamma my-indiv))) (map :anxiety (leval/gamma-statehistory-mario (indiv-to-gamma my-indiv)))))
(defn inject-0s
	"this is an artifact of me only measuring anxiety at points of anxiety, so I miss out on all the nice 0's, which look good on a chart. But I can infer where they ought to exist."
	[[x1 y1] [x2 y2]]
	(if (and x2 (= 1 y2))
		#_"This is how you do the 'real' way, with total flatness between rhythm groups. Not as nice to look at though."
		#_[[x1 y1] [(+ (* 0.9 x1) (* 0.1 x2)) 0] [(+ (* 0.1 x1) (* 0.9 x2)) 0]]
		[[x1 y1] [(+ (* 0.3 x1) (* 0.7 x2)) 0]]
		[[x1 y1]]))

(defn get-anxiety-coords
	"gets the proper [x y] coordinates for an anxiety run"
	[indiv]
	(let [gamma (indiv-to-gamma indiv)
				nonzeroed-coords (map vector (concat [0] (map :x gamma))
															(map :anxiety (leval/gamma-statehistory-mario gamma)))]
		(mapcat inject-0s nonzeroed-coords (rest nonzeroed-coords))))

(defn graph-anxiety-indiv
	"this is garbage."
	[indiv]
	(let [gamma-states (indiv-to-gamma indiv)
				anx-coords (get-anxiety-coords indiv)]
		(-> (xy-plot (sort
									(map #(clamp (+  % (rand-gauss 0.1)) 0 265)
											 (map first anx-coords)))
								 (map second anx-coords)								 
								 :x-label "t"
								 :y-label "Anxiety")
				#_(add-function #($= (-4) * ((% / (2 * 1.0)) - 1) * (% / (2 * 1.0))) 0 2.5)
				#_(incanter.charts/set-stroke :series 0 :width 2 :dash 5)
		 
				(gm/set-nate-theme)
				)))

(defn view-anxiety-indiv [indiv]
	(view (graph-anxiety-indiv indiv)))

(defn save-anxiety-indiv [indiv filename]
	(save-pdf (graph-anxiety-indiv indiv)
						(str "level_saves/" filename ".pdf")
						:width 800
						:height 300))

#_(from-str (file-to-str "level_saves/dogtest.txt"))
(defn save-evo-data [indiv filename-root]
	(let [evo-data {:generation @generation
									:time (time-relative)
									:last-improvement (last @fitplot)
									:params @params
									:fitplot @fitplot
									:genotype-size (count (:genotype indiv))
									:gmodel (repl/source 'leval/varying-gmodel)
									:indiv indiv}]
		(str-to-file (to-string evo-data) (str "level_saves/" filename-root ".txt"))))

(defn save-all-stats [root-filename]
	(let [indiv @current-best-indiv
				[g f t] (last @fitplot)
				filename (str root-filename
											"_g" g
											"_f" (float (/ (int (* f 100)) 100))
											"_t" t
											"_thresh" (:gene-count-penalty-threshold @params))]
		(do
			(save-anxiety-indiv indiv filename)
			(draw-level-indiv indiv filename)
			(save-evo-data indiv filename))))

(defn current-stats [] [@generation (last @fitplot) @current-best-indiv])

;slime-list-threads 
;slime-thread-kill (when your cursor is on the thread you want to DIE)
;... or just press 'k' when you're in the thread list.

(defn graph-fitness-improve
	"this is garbage."
	[filename]
	(let [prefix "level_saves/small zelda levels/"]
		(save-pdf
		 (let [timeline (take-while
										 #(< (first %) 292)
										 (drop
											1
											(first (:fitplots (from-str ;<-change to :fitplot for Mario (strip "first" too)
																				 (file-to-str
																					(str prefix
																							 filename)))))))]
			 (-> (xy-plot (map first timeline)
										(map (comp (fn [y] (- y)) second) timeline)	;<-sub from 40 for Mario.						 
										:x-label "Generation"
										:y-label "Fitness")
					 #_(add-function #($= (-4) * ((% / (2 * 1.0)) - 1) * (% / (2 * 1.0))) 0 2.5)
					 #_(incanter.charts/set-stroke :series 0 :width 2 :dash 5)
		 
					 (gm/set-nate-theme)
					 ))
		 (str prefix filename "_fitplot.pdf")
		 :width 800
		 :height 300))
	)

(defn graph-fitness-improve-for-that-screwed-up-one
	"this is garbage."
	[filename]
	(let [prefix "level_saves/"]
		(save-pdf
		 (let [timeline (drop
										 1
										 (first (:fitplots (from-str ;<-change to :fitplot for Mario (strip "first" too)
																				(file-to-str
																				 (str prefix
																							filename))))))]
			 (-> (xy-plot (map first timeline)
										(map (comp (fn [y] (- y)) second) timeline)	;<-sub from 40 for Mario.						 
										:x-label "Generation"
										:y-label "Fitness")
					 #_(add-function #($= (-4) * ((% / (2 * 1.0)) - 1) * (% / (2 * 1.0))) 0 2.5)
					 #_(incanter.charts/set-stroke :series 0 :width 2 :dash 5)
		 
					 (gm/set-nate-theme)
					 ))
		 (str prefix filename "_fitplot.pdf")
		 :width 800
		 :height 300))
	)
