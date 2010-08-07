;; imageutils

;; imagetools

(ns nathansorenson.imageutils
	(:refer-clojure)
	(:use [nathansorenson nutils])
	(:import 	(javax.imageio ImageIO)
						(java.awt.image BufferedImage)
						(java.awt.geom GeneralPath)
						(java.awt BasicStroke Color)
						(java.io File)
						(java.nio ByteBuffer)
						))

(defn bytevec-to-int
	"converts a vector of 3 chars into the single int fmt used by Java BufferedImage"
	[[r,g,b]]
		(bit-or (bit-or (bit-shift-left r 16) (bit-shift-left g 8)) b))
		
(defn getRGB
	"(BufferedImage, [x y]) returns a vector [r g b] from pixel coordinates, 0.0-1.0"
	[#^BufferedImage image [#^Integer x #^Integer y]]
	(let [pixel (.getRGB image (int x) (int y))]
		(vector (/ (bit-and 0xFF (bit-shift-right pixel 16)) 255.0)
						(/ (bit-and 0xFF (bit-shift-right pixel  8)) 255.0)
						(/ (bit-and 0xFF pixel) 255.0))))

(defn setRGB
	"takes floating point colors 0.0-1.0 draws them to BufferedImage"
	[#^BufferedImage image #^Integer x #^Integer y 
	 #^Float r #^Float g #^Float b]
		(.setRGB image x y (bytevec-to-int 	[(byte (clamp (* r 255) 0 255))
																				 (byte (clamp (* g 255) 0 255))
																				 (byte (clamp (* b 255) 0 255))])))
		 
(defn save-image [bi filestr]
	"saves as a png only for now. Please give it the right extension."
	(ImageIO/write bi "PNG", (File. filestr)))

(defn load-image [filestr]
	"returns a BufferedImage from the filename given."
	(ImageIO/read (File. filestr)))
	
(defn new-image [width height]
	(BufferedImage. width height BufferedImage/TYPE_INT_RGB))
	

(defn MSE
	"caclulates MSE error of two BufferedImages"
	[#^BufferedImage img1 #^BufferedImage img2]
	(let [width (min (.getWidth img1) (.getWidth img2))
				height (min (.getHeight img1) (.getHeight img2))
				;diff is a grid of RGB triplet differences squared
				diff (for [x (range width) y (range height)] 
								(map #(Math/pow % 2) (map - (getRGB img1 [x,y]) (getRGB img2 x,y))))
				;add up each component error so we get a single list of error values
				sumdiff (map + (map first diff) (map second diff) (map third diff))]
		;add up every error and divide by total for mean.
		(/ (reduce + sumdiff) (* width height 3.0))))


(defn bi-test
	"draws to a buffered image. So much easier!"
	[]
	(let [bi (BufferedImage. 100 100 BufferedImage/TYPE_INT_RGB)
				g (.createGraphics bi)
				p (GeneralPath.)]
		(doto p
			(.moveTo 10.0, 10.0)
			(.lineTo 20.0, 20.0)
			(.lineTo 20.0, 30.0)
			(.lineTo 30.0, 40.0))
		(doto g
			(.setPaint Color/BLUE)
			(.setStroke (BasicStroke. 5.0))
			(.draw p))
		;(save-BufferedImage bi)
		))
			
(defn stroke-to-path
	"creates a GeneralPath from an [x y] or (x y) pointlist"
	[pointlist]
	(let [gp (new GeneralPath)]
		(.moveTo gp (double (first (first pointlist))), (double(second (first pointlist))))
		(doseq [p (rest pointlist)]
			(.lineTo gp (float (first p)), (float (second p))))
		gp))
		
(defn draw-line
	"draws a line, with a list of [x y] or (x y) points"
	[bufferedimage pointlist]		
		(doto (.createGraphics bufferedimage)
			(.setPaint Color/RED)
			(.setStroke (BasicStroke. 1.0))
			(.draw (stroke-to-path pointlist)))
		bufferedimage)

(defn draw-labels
	"draws the labels, with a list of [x y label] points."
	[bufferedimage pointlabels]		
		(let [g (.createGraphics bufferedimage)]
			(.setPaint g Color/RED)
			(doseq [ptl pointlabels]
				(.drawString g (.toString (last ptl)) (first ptl) (second ptl))))
		bufferedimage)
		
;???? where did this duplicate come from?
;(defn draw-line
;	"draws a line, with a list of [x y] or (x y) points"
;	[bufferedimage pointlist]		
;		(doto (.createGraphics bufferedimage)
;			(.setPaint Color/RED)
;			(.setStroke (BasicStroke. 1.0))
;			(.draw (stroke-to-path pointlist)))
;		bufferedimage)		
			
(defn draw-img
	"draws a bufferedimage onto another buffered image"
	[bufferedimage bi x y]		
	(let [g (.createGraphics bufferedimage)]
		(.drawImage g bi x y nil))
	bufferedimage)

(defn bounds
	"gets bounds of a vector of points. returns [minx miny maxx maxy]"
	[points]
	(let [minx (apply min (map first points))
				maxx (apply max (map first points))
				miny (apply min (map second points))
				maxy (apply max (map second points))]
		[minx miny maxx maxy]))
			 
(defn scale-to-image
	"scales a vector of points to fit within a buffered image. Vertically Flips values."
	[points bi]
	(let [[minx miny maxx maxy] (bounds points)
        width (- maxx minx)
        height (- maxy miny)
        xratio (/ (.getWidth bi) width)
        yratio (/ (.getHeight bi) height)
        r (map #(vector (int (* (- (first %) minx) xratio)), 
                        (- (.getHeight bi) (int (* (- (second %) miny) yratio))))
               points)]
		(if (vector? points)
      (vec r)
      r)))
