(ns nathansorenson.window
	(:import (javax.swing JFrame SwingUtilities)					 
					 (java.awt Color Graphics Canvas GraphicsEnvironment
										 GraphicsDevice GraphicsConfiguration Graphics2D)
					 (java.awt.image BufferedImage BufferStrategy)))

(def dim-screen  [1200  220])
;(def num-bins-x 3)
;(def num-bins-y 3)
;(def points-per-bin 70)
(def *go* true)
;(def pointlist (atom (take 5000 (repeatedly #(vector (rand-int (dim-screen 0))

;------- drawing/saving things moved from GA.clj ------------------------

(def *bestimage* (ref (new-image 10 10)))
(defn save-best
	[]
	(save-image (draw-individual (currentbest)) "levels/aout.png"))

(defn update-image
	[]
	(dosync (ref-set *bestimage* (draw-individual (currentbest)))))

;------------------Rendering details below-------------------------

(defn render-scene [g2d buffer bi]
  (doto #^Graphics2D g2d
    (.setColor Color/BLUE)
    (.fillRect 0 0 (dim-screen 0) (dim-screen 1))
		(.drawImage @*bestimage* 0 -100 Color/BLUE nil)
		)
	
  (.drawImage (.getDrawGraphics #^BufferStrategy buffer) bi 0 0 nil)
  (when (not (.contentsLost #^BufferStrategy buffer))
    (.show #^BufferStrategy buffer)))

(defn activity-loop [canvas]
  (let [buffer (.getBufferStrategy canvas)
        bi     (.. (GraphicsEnvironment/getLocalGraphicsEnvironment)
                   (getDefaultScreenDevice) (getDefaultConfiguration)
                   (createCompatibleImage (dim-screen 0) (dim-screen 1)))
        g2d    (.createGraphics bi)]
    (while *go*
			(Thread/sleep 500)
     ; (swap! pointlist step) swap in the new image
      (render-scene g2d buffer bi)))) ;@image ref here?

(defn gowindow []
	(future (SwingUtilities/invokeLater
	 (let [frame   (JFrame.)
				 canvas  (doto (Canvas.) (.setIgnoreRepaint true)
											 (.setSize (dim-screen 0) (dim-screen 1)))
				 window  (doto (JFrame.) (.add canvas) .pack .show
											 ;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE) ;kills swank.
											 (.setIgnoreRepaint true))]
		 (.createBufferStrategy canvas 2)
		 (activity-loop canvas))))) ;not using future...