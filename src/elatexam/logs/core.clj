(ns elatexam.logs.core
  (:use 
    elatexam.logs.util
    [clojure.contrib.io :only (read-lines)])
  (:require 
    [clojure.string :as string]))

(defn property-name [s]
  (when s
    (let [[k v] (string/split s #"=")]
      (when v k))))

(defn property-value [s]
  (let [[k v] (string/split s #"=")]
     (when v (string/trim-newline v))))

(def valid-pnames #{"id" "page" "todo" "save" "hashCode" "submit" "task" "studentAnnotation" "action" "try"})

(defn properties-to-map [lines]
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

(def invalid-users #{"a" "ab" "aberger" "ahlborn" "astudent" "aufsicht" "bojack" "js.test" "reech" "sdienst" "test" "testa" "teststudi" "wolltest" 
                     "rublack1" "Schminder" "schwendel" "sdienst@informatik.uni-leipzig.de"})

(defn parse-log-entry 
  "Create a map with entries :timestamp, :user, :ip 
   as well as entries with textual keys that resemble the log entry contents."
  [lines]
  (let [[[_ timestamp username ip]] (re-seq #"(.*) INFO .* (.+)@(.*): .*" (first lines))
        details  (prop-to-map (next lines))]
    (assoc details :user username :timestamp timestamp :ip ip)))

	
;; log-entries contains a sequence of maps
(defn log-entries 
  "Read an elatexam complextask logfile. Creates a sequence of maps
that represent each log entry. Mandatory keys are:
:timestamp, :user, :ip."
  [& filenames]	
  (let [entries (->> filenames   
                  (mapcat read-lines)
                  (partition-when #(re-find #"\d{4}-\d\d-\d\d" %))
                  (pmap parse-log-entry)
                  (remove (comp nil? :ip))
                  (remove invalid-users)
                  (sort-by (comp parse-time :timestamp)))]
    (distinct-by #(dissoc % :timestamp) entries)))
 
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

(defn users 
  "All unique user names"
  [log-entries]
  (distinct (map user log-entries)))

(defn user-entries 
  "Group log entries that belong to the same user."
  ([log-entries] (group-by :user  log-entries))
  ([log-entries name] (filter #(= name (user %)) log-entries))) 

(defn group-by-id [log-entries]
  (group-by taskid log-entries))

(defn exams-by-user 
  "Map of users to map of taskids to seq. of log entries.
TODO: Identify incomplete exams."
  [log-entries]
  (map-values group-by-id (user-entries log-entries)))

(defn time-differences 
  "Return time differences between successive log entries in milliseconds."
  [log-entries]
  (let [timestamps       (map (comp parse-time :timestamp) log-entries)
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

(defn logs-from-dir 
  "Load all complexTaskPosts.log.* files within the given directory."
  [dir]
  (apply log-entries (files-in dir #".*complexTaskPosts.log.*")))

(comment
  (def le (log-entries "input/complexTaskPosts.log"))
  (nth le 6)

  (def entries (logs-from-dir "d:/temp/e"))
  (def users (user-entries entries))
  (count (keys users))
  
  (map millis-to-string (time-differences (user-entries (log-entries "0902.log") "haferstroh")))
  
  
  
  
  )
;(millis-to-time-units (- (.getTime (java.util.Date.)) (.getTime (java.util.Date. 81 7 19 16 0))))

;test
