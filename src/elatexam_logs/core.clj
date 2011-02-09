(ns elatexam-logs.core
  (:use 
    elatexam-logs.util
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

(def valid-pnames #{"id" "page" "todo" "save" "hashCode" "submit" "task"})

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
                  (sort-by (comp parse-time :timestamp)))]
    (distinct-by #(dissoc % :timestamp) entries)))
 
(defn users [log-entries]
  (distinct (map :user log-entries)))

(defn user-entries 
  "Group log entries that belong to the same user."
  ([log-entries] (group-by :user  log-entries))
  ([log-entries user] (filter #(= user (:user %)) log-entries))) 

(defn time-differences 
  "Return time differences between successive log entries in milliseconds."
  [log-entries]
  (let [timestamps       (map (comp parse-time :timestamp) log-entries)
        time-differences (map #(Math/abs (apply - %)) (partition 2 1 timestamps))]
    time-differences))

(comment
  (def le (log-entries "input/complexTaskPosts.log"))
  (nth le 6)

  (def e (apply log-entries (files-in "input")))
  
  (map millis-to-string (time-differences (user-entries (log-entries "0902.log") "haferstroh"))))
;(millis-to-time-units (- (.getTime (java.util.Date.)) (.getTime (java.util.Date. 81 7 19 16 0))))

;test
