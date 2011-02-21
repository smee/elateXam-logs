(ns elatexam.logs.util
  (:use 
    [clojure.java.io :only (reader)]
    [clojure.contrib.seq :only (indexed)]
    [clojure.contrib.string :only (replace-char)]))

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

(def ^java.text.SimpleDateFormat dateformat (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss,SSS"))

(defn parse-time [^String s]
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

(defn take-to-first
  "Returns a lazy sequence of successive items from coll up to
  and including the point at which it (pred item) returns true.
  pred must be free of side-effects."
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
       (if-not (pred (first s))
         (cons (first s) (take-to-first pred (rest s)))
         (list (first s))))))

(defn partition-when
  "Applies f to each value in coll, splitting it each time f returns
   true. Returns a lazy seq of lazy seqs."
  [f coll]
  (when-let [s (seq coll)]
  (lazy-seq
    (let [run (take-to-first f s)
          res (drop (count run) s)]
        (cons run (partition-when f res)))))) 

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

(defn- fix-path [s]
  (replace-char \\ \/ s))

(defn files-in 
  "Seq of all files in dir. Optionally specify regular expression that must match the filename (incl. path)"
  ([dir] (files-in dir #".*"))
  ([dir pattern] (filter (memfn isFile)   
                 (for [file (-> dir java.io.File. file-seq) 
                       :when (re-matches pattern (fix-path (.getName file)))]
                   file))))

(defn find-file 
  "Traverse directory dirpath depth first, return all files matching
the regular expression pattern"
  [dir pattern]
)

(defn map-values 
  "Change all map values by applying f to each one."
  [f m]
  (into {} (for [[k v] m] [k (f v)])))
(defn map-keys 
  "Change all map keys by applying f to each one."
  [f m]
  (into {} (for [[k v] m] [(f k) v])))

(defn dissoc-where-v 
  "Remove all mappings [ k v] where (f v) is logically true."
  [f m]
  (into {} (for [[k v] m] (when (not (f v)) [k v]))))

(defn s2f 
  "Parse float string representation."
  [s]
  (Float/parseFloat s))

(defn s2i 
  "Parse integer string representation."
  [s]
  (Integer/parseInt s))
(defn s2l 
  "Parse long string representation."
  [s]
  (Long/parseLong s))