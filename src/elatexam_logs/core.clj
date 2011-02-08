(ns elatexam-logs.core
  (:use elatexam-logs.util))

(defn property-name [^String string]
  (when string
    (let [[k v] (.split string "=")]
      (when v k))))

(defn property-value [^String string]
  (let [[k v] (.split string "=")]
     v))

(def valid-pnames #{"id" "page" "todo" "save" "hashCode" "submit"})

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

(defn parse-log-entry 
  "Create a map with entries :timestamp, :user, :ip 
   as well as entries with textual keys that resemble the log entry contents."
  [lines]
  (let [[[_ timestamp username ip]] (re-seq #"(.*) INFO .* ([a-z0-9]+)@(.*): .*" (first lines))
        details  (properties-to-map lines)]
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
  (let [lines   (read-lines filename)
        chunks  (partition-by #(re-find #"\d{4}-\d\d-\d\d" %) lines)
        entries (map (partial apply concat) (partition 2 chunks))]
    (map parse-log-entry entries)))
 
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
  (def le (log-entries "input/complexTaskPosts.log"))
  (time-differences (user-entries (log-entries "0902.log") "haferstroh")))
;(millis-to-time-units (- (.getTime (java.util.Date.)) (.getTime (java.util.Date. 81 7 19 16 0))))

;test
