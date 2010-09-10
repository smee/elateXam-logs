(ns elatexam-logs.core
  (:use [clojure.contrib.duck-streams :only (read-lines)]))

(defn property-name [string]
  (aget (.split string "=") 0))

(defn property-value [string]
  (let [arr (.split string "=")]
    (if (= 1 (count arr))
      ""
	  (aget arr 1))))

(defn parse-log-entry 
  "Create a map with entries :timestamp, :user, :ip 
   as well as entries with textual keys that resemble the log entry contents."
  [[first-seq second-seq]]
  (let [[[_ timestamp username ip]] (re-seq #"(.*) INFO .* ([a-z0-9]+)@(.*): .*" (first first-seq))
	    sane-second-seq (drop-last second-seq)
        details  (reduce #(assoc %1 (property-name %2) (property-value %2)) 
                       {} 
					   sane-second-seq)]
	(assoc details :user username :timestamp timestamp :ip ip)))

	
(def dateformat (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss,SSS"))

(defn millis-to-time-units
  "Convert time in milliseconds to a map that contains
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

(defn millis-to-string [time]
  (let [time-unit-map (millis-to-time-units time)]
    ()))

		

	
;; log-entries contains a sequence of maps
(defn log-entries 
  "Read an elatexam complextask logfile. Creates a sequence of maps
that represent each log entry. Mandatory keys are:
:timestamp, :user, :ip."
  [filename]	
  (let [lines  (read-lines filename)
        chunks (partition-by #(re-find #"\d{4}-\d\d-\d\d" %) lines)
        pairs  (partition 2 chunks)]
    (map parse-log-entry pairs)))
 
(defn user-entries 
  "Filter log entries that belong to the same user."
  [log-entries username]
  (filter #(= username (:user %)) log-entries)) 

(defn time-differences 
  "Return time differences between successive log entries in milliseconds."
  [log-entries]
  (let [timestamps       (map #(.parse dateformat (:timestamp %)) log-entries)
        time-differences (map #(Math/abs (apply - %)) (partition 2 1 (map #(.getTime %) timestamps)))]
    time-differences))

(comment
  (time-differences (user-entries (log-entries "0902.log") "haferstroh")))
;(millis-to-time-units (- (.getTime (java.util.Date.)) (.getTime (java.util.Date. 81 7 19 16 0))))

;test
