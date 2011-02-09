(ns elatexam-logs.util
  (:use 
    [clojure.java.io :only (reader)]
    [clojure.contrib.seq :only (indexed)]))

(defn read-lines-enc
  "Like clojure.core/line-seq but opens f with reader. An encoding may be specified, too.
 Automatically closes the reader AFTER YOU CONSUME THE ENTIRE SEQUENCE."
  ([f] (read-lines-enc f "UTF-8"))
  ([f encoding]
    (let [read-line (fn this [^java.io.BufferedReader rdr]
                      (lazy-seq
                        (if-let [line (.readLine rdr)]
                          (cons line (this rdr))
                          (.close rdr))))]
      (read-line (reader f :encoding encoding)))))

(def dateformat (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss,SSS"))

(defn parse-time [s]
  (.getTime (.parse dateformat s)))

(defn time-to-string [t]
  (.format dateformat (java.util.Date. t)))

(defn millis-to-time-units
  "Convert time in milliseconds to a seq that contains
entries for different time units: :seconds, :minutes, :hours, :days"
  
  ([time-in-msec] 
    (millis-to-time-units 
      []
      [52 7 24 60 60 1000 1] 
      time-in-msec))
  
  ([result time-unit-durations rest-time]
    (if (empty? time-unit-durations)
      (interleave [:years :weeks :days :hours :minutes :seconds :milliseconds] result)
      ;else
      (let [duration-in-msec (apply * time-unit-durations)]
        (recur 
          (conj result (quot rest-time duration-in-msec))
          (rest time-unit-durations)
          (rem rest-time duration-in-msec))))))

(defn millis-to-string 
  "String representation of the time in msec. Uses only time modulo 24h."
  [time]
  (let [m   (apply hash-map (millis-to-time-units time))
        i2s (fn [i] (if (< i 10) (str "0" i) (str i)))]
    (str (i2s (:hours m)) \: (i2s (:minutes m)) \: (i2s (:seconds m)) \, (i2s (:milliseconds m)))))


(defn index-filter [pred coll]
  (when pred
    (for [[idx elt] (indexed coll) :when (pred elt)] idx)))

(defn partition-when [pred coll]
  (let [coll-end (count coll)
        indices (index-filter pred coll)
        index-pairs (partition 2 1 [coll-end] indices)
        coll-vec (vec coll)]
    (for [[start end] index-pairs] (subvec coll-vec start end))))

(defn starts-with-any 
  "Does the string s start with any string within str-set?"
  [str-set ^String s]
  (some #(.startsWith s %) str-set))

(defn distinct-by
  "Returns a lazy sequence of object with duplicates removed,
  where duplicates are defined by applying the function func to each item.
  Calling (distinct-by _ identity) is equivalent to (clojure.core/distinct _)."
  [func coll]
    (let [step (fn step [xs seen]
                 (lazy-seq
                   ((fn [[f :as xs] seen]
                      (when-let [s (seq xs)]
                        (let [f-val (func f)]
                          (if (contains? seen f-val) 
                            (recur (rest s) seen)
                            (cons f (step (rest s) (conj seen f-val)))))))
                     xs seen)))]
      (step coll #{})))

(defn files-in 
  "Seq of all files in dir."
  [dir]
  (filter (memfn isFile) (file-seq (java.io.File. dir))))