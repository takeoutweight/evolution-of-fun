

;; fileio
; For writing to the XML data used by Super Maryo (the open-source clone).

(ns nathansorenson.fileio
	(:refer-clojure)
	(:use clojure.contrib.prxml
				nathansorenson.flexencoding)
	(:require [clojure.zip :as zip]))
	
(defn write-xml
  [xml filename]
  (with-open [f (java.io.FileWriter. filename)]
    (binding [*out* f] (clojure.xml/emit xml))))
	
(defn emptyxml [] (clojure.xml/parse "../../levels/empty.smclvl"))

(defn block-tag
	"returns an xml tag of a given [x,y] block"
	[[x,y]]
	{:tag :sprite, 
		:attrs nil, 
		:content [
			{:tag :Property, :attrs {:name "posx", :value (str x)}, :content nil} 
			{:tag :Property, :attrs {:name "posy", :value (str y)}, :content nil} 
			{:tag :Property, :attrs {:name "image", :value "ground/green_3/ground/top/1.png"}, :content nil} 
			{:tag :Property, :attrs {:name "type", :value "massive"}, :content nil}]})

(defn enemy-tag
	"returns an xml tag of an enemy character"
	[[x,y]]
	{:tag :enemy, 
		:attrs nil, 
		:content [
			{:tag :Property, :attrs {:name "posx", :value (str x)}, :content nil} 
			{:tag :Property, :attrs {:name "posy", :value (str y)}, :content nil} 
			{:tag :Property, :attrs {:name "type", :value "turtle"}, :content nil} 
			{:tag :Property, :attrs {:name "color", :value "red"}, :content nil}
			{:tag :Property, :attrs {:name "direction", :value "left"}, :content nil}]})

(defn player-tag 
	"This is the player tag format"
	[[x,y]]
	{:tag :player,
			:content [{:tag :Property :attrs {:name "posx" :value (str x)}}
								{:tag :Property :attrs {:name "posy" :value (str y)}}
								{:tag :Property :attrs {:name "direction" :value "right"}}]})
								
(defn stopper-tag 
	"This is the player tag format"
	[[x,y]]
	{:tag :enemystopper,
			:content [{:tag :Property :attrs {:name "posx" :value (str x)}}
								{:tag :Property :attrs {:name "posy" :value (str y)}}]})

(defn zip-home
	"zips all the way back to the root, returns the ZIPPER."
	[zipper]
	(let [up (clojure.zip/up zipper)]
		(if (nil? up)
			zipper
			(recur up))))
	
(defn find-tag
	"returns the loc of the first XML tag it finds. nil if not found."
	[zipper tag]
		(if (= (:tag (zip/node zipper)) tag)
			zipper
			(if (zip/end? zipper)
				nil
				(recur (zip/next zipper) tag))))
				
(defn find-start
	"finds [x,y] coords of the first block of the level."
	[level]
	[0 0])
;(find-start '({:type :enemy}, {:type :block, :coords [10, 42]}))
			
(defn set-player-start
	"finds the player tag and updates it to reflect the start block. Alters the zipper."
	[zipper level]
	(let [plyr (find-tag zipper :player)]
		(if (nil? plyr)
			(zip/append-child zipper (player-tag (find-start level)))
			(zip/replace plyr (player-tag (find-start level))))))
	
(defn zipper-add-blocks
	"adds level blocks into a zipper. Expects just a seq of [x,y] blocks 
	 Returns a zipper with level added."
	[zipper blocks]
	(if (empty? blocks)
			zipper
			(recur (zip/append-child zipper (block-tag (first blocks))) (rest blocks))))
			
(defn zipper-add-enemies
	"adds enemies into a zipper. Expects just a seq of [x,y] enemy positions 
	 Returns a zipper with enemies added."
	[zipper enemies]
	(if (empty? enemies)
		zipper
		(recur (zip/append-child zipper (enemy-tag (first enemies))) (rest enemies))))

(defn zipper-add-stoppers
	"adds enemies into a zipper. Expects just a seq of [x,y] enemy positions 
	 Returns a zipper with enemies added."
	[zipper stoppers]
	(if (empty? stoppers)
		zipper
		(recur (zip/append-child zipper (stopper-tag (first stoppers))) (rest stoppers))))

(defn save-level
	"saves a level to a playable file"
	[level filename]
	(let [xml (-> (emptyxml)
								(zip/xml-zip)
								(set-player-start level)
								(zip-home)
								(zipper-add-blocks (flex-to-block-level level))
								(zip-home)
								;(zipper-add-enemies (to-enemies level))
								(zip-home)
								;(zipper-add-stoppers (to-stoppers level))								
								(zip/root))]
		(write-xml xml filename)))		
									
(defn has-value?
	"Does a tag contain a tag with the 'value' proprety?"
	[tag value]
		(if (some #(= (:value (:attrs %)) value) (:content tag))
			true
			false))
			
(defn get-content-value
	"gets attribute 'value' (named by name='name')"
	[tag name]
		(let [cont (:content tag)
				  pairs (for [{attr :attrs} cont] [(:name attr), (:value attr)])]
			(loop [ps pairs]
				(if (empty? ps)
					nil
					(if (= (first (first ps)) name)
						(second (first ps))
						(recur (rest ps)))))))
;(get-content-value (player-tag 2 3) "direction") ;should equal "right"
			
(defn derive-block
	"takes a ziper loc and returns a [x y] block, if possible. Else nil."
	[ziploc]
	(let [tag (zip/node ziploc)]
		(if (has-value? tag "massive")
			[ (Integer/parseInt (get-content-value tag "posx")),
			  (Integer/parseInt (get-content-value tag "posy")) ]
			nil)))
 
(defn derive-level
	"takes a xml zip and searches for everything that could be a block, returns the vector"
	[zip]
  (loop [z (zip/down zip), level []]
  	(if (zip/end? z)
  		level
  		(let [block (derive-block z)
  				 	nlevel (if (nil? block) level, (conj level block))]	
  			(recur (zip/next z) nlevel)))))

(defn load-level
	"parses an xml level"
	[filename]
	(-> (clojure.xml/parse filename), (zip/xml-zip) , (derive-level)))
	
;(def testlevel [[0,536][64,536][128,536][192,536][256,536]])
;(save-level testlevel "levels/out.smclvl")
