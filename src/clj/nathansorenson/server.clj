(ns nathansorenson.server
  (:refer-clojure)
  (:use [incanter.core :only [$=]])
  (:require 
   [nathansorenson.nutils :as nu]
   [clojure.contrib.server-socket :as ss])
  (:import 
   (java.io BufferedReader InputStreamReader OutputStreamWriter)
   (java.net Socket)
   (java.util.concurrent CountDownLatch))
  )
(def connections (ref [])) ;[in out]
(def jobs (ref [])) ;[{:obj-ref (ref obj) :latch _}]

(defn grab-first
  "pops the first item off a ref'd vector."
  [ref]
  (dosync (when-let [c (first (ensure ref))]
            (alter ref (comp vec rest))
            c)))

(defn grab-all
  "returns the entire ref'd vector, sets ref to []"
  [ref]
  (dosync
   (let [all (ensure ref)]
     (ref-set ref [])
     all)))

(def comm-errors (atom []))
(defn post-job
  "Todo... we can't distinguish clients yet. SOME client will grab the job and return
   its answer. There is no routing needed at this point."
  [obj]
  (let [latch (CountDownLatch. 1)
        send-fn (fn []
                  (if-let [[in out] (grab-first connections)]
                    (do (binding [*in* in
                                  *out* out]
                          (let [s (nu/to-string obj)]
                            (when (= s "") ;a stab-in-the-dark attempt to track down a bug.
                              (swap! comm-errors conj obj))
                            (println s))
                          (flush)       
                          (let [ans (nu/from-str (read-line))] ;wake up pending jobs.
                            (dosync (alter connections conj [in out]))
                            (doall (map #(.countDown %) (grab-all jobs))) ;... this is probably bad... can I countdon an already-counted-down latch?
                            (if (not= ans ::error)
                              ans
                              (do
                                (swap! comm-errors conj :err-response)
                                ::failed-connection))))) ;loop on fail.. *shrug*
                    ::failed-connection))]
    (loop [res (send-fn)] ;not beautiful... maybe a slicker way?
      (if (= res ::failed-connection)
        (do
          (dosync (alter jobs conj latch))
          (.await latch)
          (recur (send-fn)))
        res))))

;a parallel dispatch of this, doesn't seem to conj the jobs to anything
;nor does it consume the connection... (or at least its returned...) and other aren't awakened.
;laziness problem.
;now I had the problem where both got sent to the client in a row... hmmm...
;but that was after a re-build... could be that compile problem? Seem OK now.
;TODO: just make the job posting actually return the value now!!

(defn evo-server
  [in out]
  (binding [*in* (BufferedReader. (InputStreamReader. in))
            *out* (OutputStreamWriter. out)]
    (do 
      (dosync (alter connections conj [*in* *out*]))
      (let [lock (Object.)] (locking lock (.wait lock))))))

;todo: we could notify waiting jobs to wake up when a new guy connects..
;but for nwo we'll assume everyone connects early.

(defn server [socket] (ss/create-server socket evo-server))

(defn connect [sock]
  (let [socket (Socket. "localhost" sock)
        in (BufferedReader. (InputStreamReader. (.getInputStream socket)))
        out (OutputStreamWriter. (.getOutputStream socket))]
    [in out]))

(def error-msg (atom []))
(defn client
  "Infinitely reads input, applies the function, and writes back."
  [socket function]
  (let [[in out] (connect socket)]
    (binding [*in* in
              *out* out]
      (while true
        (let [rl (read-line)]
          #_(println rl) ;no more echoing...
          (try
            (println (nu/to-string (function (nu/from-str rl))))
            (catch Exception e (swap! error-msg conj rl))))
        (flush)))))

(defn message [[in out] m]
  (binding [*in* in
            *out* out]
    (println m)
    (flush)))
