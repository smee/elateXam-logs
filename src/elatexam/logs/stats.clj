(ns elatexam.logs.stats
  (:use 
    [clojure.set :as cs]
    [org.clojars.smee.map :only (map-values)]
    [incanter.core :only ($=)])
  (:require 
    [incanter.stats :as stats]
    [elatexam.logs.core :as c]))

(defn nan? [i]
  (Double/isNaN i))
(defn sum [coll]
  (reduce + coll))

;;;;;;; individual subtaskdef difficulty ;;;;;;;;;;;;;;;;;;;;;;;;
(defn points-of 
  "Calculate the final score of a subtasklet. Uses either the auto
or the mean of the manual points."
  [subtasklet]
  (let [ap (:auto-points subtasklet)
        mp (:manual-points subtasklet)]
    (cond 
      ap ap
      (not-empty mp) (/ (sum mp) (count mp))
        :else Double/NaN)))

(defn points-reached-percent [std]
  (/ (points-of std) (:points std)))

(defn sum-all-points [subtasklets]
  (sum (map points-of subtasklets)))


(defn task-stats 
  "Calculate map of subtaskdef ids to seq of subtasklets of students that
were presented this subtaskdef."
  [subtaskdefs tries]
  (let [std-points     (cs/project subtaskdefs [:id :points])
        joined  (map #(cs/join std-points (:subtasklets %)) tries)
        exams   (map (partial group-by :id) joined)]
    (apply (partial merge-with concat) exams)))


(defn task-difficulty 
  "Calculate map of taskdef ids to mean of points reached in tries."
  [subtaskdefs tries]
  (let [questions (task-stats subtaskdefs tries)]
    (map-values (comp stats/mean (partial map points-reached-percent)) questions)))

(defn- easy? [x]
  (<= 0.8 x))
(defn- hard? [x]
  (>= 0.4 x))
 
(defn assess-difficulty 
  "Maps a difficulty to a task difficulty value. 
d<0.4 is :hard,
d>0.8 is :easy"
  [x]
  (cond 
    (easy? x) :easy
    (hard? x) :hard
    :else     :medium))

(defn cluster-difficulties 
  "Split collection of task difficulties into easy, medium and hard categories."
  [td]
  (->> td
    (group-by (comp assess-difficulty second))
    (map-values (partial into {}))))

(defn difficulty-stats 
  "Calculates points and number of easy, medium and difficult questions per user."
  [subtaskdefs tries]
  (let [task2diff (->> tries 
                    (task-difficulty subtaskdefs)
                    (map-values assess-difficulty))]
    (set (for [{u :user st :subtasklets} tries] 
           (let [points (sum-all-points st)] 
             (merge 
               (hash-map  :user u :points points)
               (frequencies (map (comp task2diff :id) st))))))))

;;;;;;;;;;;;;;;;;; discrimination power (Trennschaerfe) ;;;;;;;;;;;;;;;;;

(defn exam-scores
  [tries]
  (into {} 
    (for [{u :user st :subtasklets} tries]
      [u (sum-all-points st)])))

(defn discrimination-power-scores 
  "Trennschärfe, correlation of item scores with exam scores without this item."
  [s tries]
  (let [question-ids (keys (task-stats s tries)) ;; ids of questions used
        ;; map of usernames to map of subtask ids to subtasklet
        students-questions (apply merge (map #(hash-map (:user %) (group-by :id (:subtasklets %))) tries))
        ;; map of usernames to exam scores
        exam-score (exam-scores tries)]
    
    (into {}
      (for [q question-ids]
        ;; calculate pairs of item score to (exam score - item score)
        (let [score-pairs (for [student (keys exam-score)]
                            (let [subtasklet (first (get-in students-questions [student q]))
                                  item-score (points-of subtasklet)]
                              [item-score (- (exam-score student) item-score)]))
              item-scores (map first score-pairs)
              exam-scores (map second score-pairs)]
          [q [item-scores exam-scores]])))))

(defn discrimination-power-spearman 
  [s tries]
  (let [scores (discrimination-power-scores s tries)]
    (map-values (fn [[is es]] (stats/spearmans-rho is es)) scores)))

(defn discrimination-power-pearson
  [s tries]
  (let [scores (discrimination-power-scores s tries)]
    (map-values (fn [[is es]] (stats/correlation is es)) scores)))

;;;;;;;;;;;;;;;;;; reliablity ;;;;;;;;;;;;;;;;;

(defn cronbach-alpha
  "see http://en.wikipedia.org/wiki/Cronbach%27s_alpha"
  [s tries]
  (let [exam-scores     (vals (exam-scores tries))
        item-scores     (map (partial map points-of) (vals (task-stats s tries)))
        sum-item-var    (sum (map stats/variance item-scores))
        total-score-var (stats/variance exam-scores)
        k (count exam-scores)]
    
    ($= (k / (k - 1) * (1 - sum-item-var / total-score-var)))))


(defn- remove-item [id]
  (fn [{st :subtasklets :as t}]
    (assoc t :subtasklets (remove #(= id (:id %)) st))))

(defn cronbach-if-deleted
  "Calculate cronbach's alpha after deleting each item."
  [s tries]
  (into {}
    (for [id (keys (task-stats s tries))]
      (let [tries-adj (map (remove-item id) tries)]
        [id (cronbach-alpha s tries-adj)]))))

(defn bad-cronbach?
  [cr c-i-d]
  (< cr c-i-d))

(defn find-bad-cronbach 
  "Find all subtaskdef ids that result in an increased cronbach's alpha
if deleted."
  [std tries]
(let [cid (cronbach-if-deleted std tries)
      cb (cronbach-alpha std tries)
      bad-ids (filter (comp (partial bad-cronbach? cb) second) cid)]
  (into {} bad-ids)))

;;;;;;;;;;; misc
(defn split-by-time 
  "TODO: refactor similar function from core"
  [tries]
  (let  [tr-sort (sort-by :start-time tries)
         intervals (map (fn [{s :start-time}] [s (+ s (* 90 60 1000))]) tr-sort)
         indices (keep-indexed #(when (not c/overlaps? %2 %3) %1) (partition 2 1 intervals))
         indices-fixed (concat [0] (map inc indices) [(count intervals)])
         lengths       (map (partial apply #(- %2 %1)) (partition 2 1 indices-fixed))
         groups        (loop [res (), [l & ls] lengths, tr tr-sort]
                         (if (nil? l)
                           res
                           (recur (conj res (take l tr)) ls (drop l tr) )))]
    groups))

(defn split-by-randomseed 
  "Splits all tries into separate groups by same random-seed. Returns only groups
of size >1, that means no individual students. If the exam was run without fixed random-seeds,
the returned seq will be empty."
  [tries]
  (let [gr (vals (group-by :random-seed tries))
        groups (remove #(= 1 (count %)) gr)]
    groups))