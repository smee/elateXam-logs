(ns elatexam.logs.core
  (:use 
    elatexam.logs.util
    [clojure.contrib.io :only (read-lines reader)])
  (:require 
    [clojure.string :as string]
    [clojure.contrib.str-utils2 :as str2])
  (:import 
    java.util.Calendar))

(defn save-page? [entry]
  (not (nil? (:hashCode entry))))
(defn start-exam? [entry]
  (= "new" (:todo entry)))
(defn submit-exam? [entry]
  (= "Abgeben" (:submit entry)))
(defn switch-page? [entry]
  (not-any? (set (keys entry)) [:submit :save :action]))


(defn user [log-entry]
  (:user log-entry))
(defn taskid [log-entry]
  (:id log-entry))
(defn timestamp [log-entry]
  (:timestamp log-entry))
(defn ip [log-entry]
  (:ip log-entry))


;;;;;;;;; Load log files ;;;;;;;;;;;;;;;;;;;;

(defn property-name 
  "Extract name from strings with structure 'name=value'."
  [s]
  (when s
    (let [[k v] (string/split s #"=")]
      (when v k))))

(defn property-value 
  "Extract value from strings with structure 'name=value'."
  [s]
  (let [[k v] (string/split s #"=")]
     (when v (string/trim-newline v))))

(def valid-pnames #{"id" "page" "todo" "save" "hashCode" "submit" "task" "studentAnnotation" "action" "try"})

(defn- properties-to-map 
  "Converts some strings to a hashmap where every key is mapped to its value. Also respects values that
span several lines."
  [lines]
  (loop [[[line-1 line] & lines] (partition 2 1 lines) res {}]
    (if (nil? line)
      res
      (let [p (property-name line)
            v (property-value line)
            p-last (property-name line-1)
            pname (or p p-last)
            value (if p v (conj (get res pname) v))]
        (recur lines (assoc res pname value))))))



(defn prop-to-map [lines]
  (let [lines (->> lines (partition-when (partial starts-with-any valid-pnames)) (map (partial string/join "\n")))]
    (zipmap (map (comp keyword property-name) lines) (map property-value lines))))

;; known test users
(def invalid-users #{"a" "ab" "aberger" "ahlborn" "astudent" "aufsicht" "bojack" "gast" "gast1" "js.test" 
                     "reech" "sdienst" "test" "testa" "test1" "teststudi" "wolltest" 
                     "rublack1" "Schminder" "schwendel" "sdienst@informatik.uni-leipzig.de"})

(defn first-line [file]
  (with-open [rdr (reader file)]
    (.readLine rdr)))

(defn parse-log-entry 
  "Create a map with entries :timestamp, :user, :ip 
   as well as entries with textual keys that resemble the log entry contents."
  [lines]
  (let [[[_ timestamp username ip]] (re-seq #"(.*) INFO .* (.+)@(.*): .*" (first lines))
        details  (prop-to-map (next lines))]
    (-> details
      (assoc :user username :timestamp timestamp :ip ip)
      (dissoc nil))))

	
(defn log-entries 
  "Read an elatexam complextask logfile. Creates a sequence of maps
that represent each log entry. Mandatory keys are:
:timestamp, :user, :ip."
  [& filenames]	
  (let [entries (->> filenames   
                  (sort-by first-line) ;; every logfile's first line starts with a date time
                  (mapcat read-lines)
                  (remove #(str2/contains? % "TimeExtensionGlobalAction")) ;; skip time extension entries
                  (partition-by #(re-find #"\d{4}-\d\d-\d\d .*" %))
                  (partition 2)
                  (map (partial apply concat))
                  (pmap parse-log-entry)
                  (remove (comp invalid-users user))
                  (map #(update-in % [:timestamp] parse-time)) ;; java.text.Dateformat is not thread safe
                  )]
    (distinct-by #(dissoc % :timestamp) entries)))
 

(defn logs-from-dir 
  "Load all complexTaskPosts.log.* files within the given directory."
  [dir]
  (apply log-entries (files-in dir #".*complexTaskPosts.log.*")))

(defn group-entries [fns log-entries]
  (group-by (apply juxt fns) log-entries))

;;;;;;;;; Log entry analysis functions ;;;;;;;;;;;;;;;;;;;;


(defn all-users 
  "All unique user names"
  [log-entries]
  (distinct (map user log-entries)))

(defn user-entries 
  "Group log entries that belong to the same user."
  ([log-entries] (group-by user  log-entries))
  ([log-entries name] (filter #(= name (user %)) log-entries)))


(defn group-by-id [log-entries]
  (group-by taskid log-entries))

(defn in-intervall? 
  "Is x within the numeric interval [start,end]?"
  [[start end] x]
  (and (>= x start) (<= x end)))

(defn overlaps? 
  "Do these numeric intervals overlap?"
  [[start1 end1 :as i1] [start2 end2 :as i2]]
  (or (in-intervall? i1 start2) (in-intervall? i1 end2) (in-intervall? i2 start1) (in-intervall? i2 end1)))


(defn time-interval 
  "Seq of start and end time of a seq of log-entries"
  [log-entries]
  (list (timestamp (first log-entries)) (timestamp (last log-entries))))

(defn time-differences 
  "Return time differences between successive log entries in milliseconds."
  [log-entries]
  (let [timestamps       (map timestamp log-entries)
        time-differences (map #(Math/abs (apply - %)) (partition 2 1 timestamps))]
    time-differences))

(defn add-durations [log-entries]
  (let [times (concat (list 0) (time-differences log-entries))]
    (map #(assoc %1 :duration %2) log-entries times)))

(defn time-per-page 
  "Get editing durations for all pages."
  [log-entries]
  (dissoc (->> log-entries
            add-durations
            (group-by :page)
            (map-values #(reduce + (map :duration %))))
    nil))

(defn exam-duration [log-entries]
  (reduce + (time-differences log-entries)))

(defn exam-ids [log-entries]
  (set (map :id log-entries)))

(defn editing-stats 
  "Count number of page saves per user."
  [entries]
  (map #(count (filter save-page? %)) (vals (user-entries entries))))

;;;;; groupings of log entries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn exams-by-user 
  "Map of users to map of taskids to seq. of log entries. Optionally a function may be specified,
that gets applied to every sequence of logentries individually.

For example: To get a map of users to map of id to exam duration:
    (exams-by-user entries exam-duration)"
  ([log-entries] (map-values group-by-id (user-entries log-entries)))
  ([log-entries f] (map-values (partial map-values f) (exams-by-user log-entries))))



(defn group-by-runs 
  "Group all log entries by overlapping time intervals. The keys are vectors of
[ earliest start time, latest finish time]."
  [log-entries]
  (let [log-traces    (vals (group-by #(vector (taskid %) (user %)) log-entries))
        sorted-traces (sort-by (comp timestamp first) log-traces)
        intervals     (map time-interval sorted-traces)
        indices       (index-filter (partial apply (complement overlaps?)) (partition 2 1 intervals))
        indices-fixed (concat [0] (map inc indices) [(count intervals)])
        lengths       (map (partial apply #(- %2 %1)) (partition 2 1 indices-fixed))
        groups        (loop [res (), [l & ls] lengths, tr sorted-traces]
                        (if (nil? l)
                          res
                          (recur (conj res (take l tr)) ls (drop l tr) )))]
    (into {}
      (for [gr groups] 
        [(time-interval (apply concat gr)) gr]))))


(defn semester-of 
  "String representation of the semester t falls in."
  [^Long t]
  (let [cal     (doto (Calendar/getInstance) (.setTimeInMillis t))
        year    (mod (.get cal Calendar/YEAR) 100)
        month   (inc (.get cal Calendar/MONTH))
        ss?     (contains? #{4 5 6 7 8 9} month)
        n2s     #(if (< % 10) (str "0" %) (str %))
        spring? #(contains? #{1 2 3} %)]
    (if ss?
      (str "SS" (n2s year))
      (if (spring? month)
        (str "WS" (n2s (dec year)) "/" (n2s (mod year 100)))
        (str "WS" (n2s year) "/" (n2s (mod (inc year) 100)))))))


(comment
  (def le (logs-from-dir "input"))
  (nth le 6)

  (def entries (logs-from-dir "d:/temp/e"))
  (def users (user-entries entries))
  (count (keys users))
  
  (map millis-to-string (time-differences (user-entries (log-entries "0902.log") "haferstroh")))
  
  
  
  
  )
(comment
  
  (defn- m [u t]
    {:user u :id 1 :timestamp t})
  (defn- m2 [u f t]
    (map (partial m u) (range f t 100)))
  (let [u1 (m2 "a" 0 1000)
        u2 (m2 "b" 500 1500)
        u3 (m2 "c" 2000 3000)]
    (group-by-runs (concat u1 u2 u3)))
  )