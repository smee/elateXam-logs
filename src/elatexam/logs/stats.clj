(ns elatexam.logs.stats
  (:use 
    [clojure.set :as cs]
    elatexam.logs.util)
  (:require 
    [incanter.stats :as stats]))

(defn nan? [i]
  (Double/isNaN i))

;;;;;;; individual subtaskdef difficulty ;;;;;;;;;;;;;;;;;;;;;;;;
(defn points-of 
  "Calculate the final score of a subtasklet. Uses either the auto
or the mean of the manual points."
  [subtasklet]
  (if-let [ap (:auto-points subtasklet)]
    ap
    (when-let [mp (:manual-points subtasklet)] (stats/mean mp))))

(defn task-difficulty 
  "Calculate map of taskdef ids to mean of points reached in tries."
  [subtaskdefs tries]
  (let [std     (cs/project subtaskdefs [:id :points])
        joined  (map #(cs/join std (:subtasklets %)) tries)
        exams   (map (partial group-by :id) joined)
        questions (apply (partial merge-with concat) exams)]
    (map-values (comp stats/mean (partial map #(/ (points-of %) (:points %)))) questions)))

(defn- easy? [x]
  (<= 0.8 x))
(defn- hard? [x]
  (>= 0.4 x))
 
(defn assess-difficulty [x]
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
  [taskdef tries]
  (let [task2diff (->> tries 
                    (task-difficulty (:subtaskdefs taskdef))
                    (map-values assess-difficulty))]
    (set (for [{u :user st :subtasklets} tries] 
           (let [points (reduce + (map points-reached st))] 
             (merge 
               (hash-map  :user u :points points)
               (frequencies (map (comp task2diff :id) st))))))))

