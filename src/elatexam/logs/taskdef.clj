(ns elatexam.logs.taskdef
  (:use 
    [clojure.contrib.zip-filter.xml :only (xml-> xml1-> attr attr=)]
    [elatexam.logs.util :only (dissoc-where-v s2f)]
    [elatexam.logs.xml :only (load-xml where-one-of?)])
    (:require
      [clojure.zip :as zip]
      [clojure.contrib.zip-filter :as zf]
      [clojure.contrib.zip-filter.xml :as zfx]
      [incanter.stats :as stats]))

;;;;;;;;; taskhandling.xml ;;;;;;;;;;;;;;;;;;;;;;;;
(defn taskdef-ids 
  "All taskDefIds."
  [x]
  (set (xml-> x :tasklet (attr :taskDefId))))

(defn correctors 
  "Names of all correctors"
  [x]
  (set (xml-> x :tasklet :manualCorrection (attr :corrector))))


(defn points-of-corrector 
  "All points of one corrector"
  [x c]
  (map s2f (xml-> x :tasklet :manualCorrection [(attr= :corrector c)] (attr :points))))

(defn points-per-corrector 
  "Map of corrector name to list of points."
  [x]
  (let [c (correctors x)
        p (map (partial points-of-corrector x) c)]
    (apply assoc {} (interleave c p))))

(defn- points-auto [tasklet]
  (map s2f (xml-> tasklet :autoCorrection (attr :points))))

(defn- points-manual [tasklet]
  (map s2f (xml-> tasklet :manualCorrection (attr :points))))

(defn points-of-user 
  "All points of a user and a specific taskdef. Returns all manual and automatic points given."
  [x user taskdef-id]
  (let [tasklet  (xml1-> x :tasklet [(attr= :taskDefId taskdef-id) (attr= :login user)])
        manual   (points-manual tasklet)
        auto     (points-auto tasklet)] 
    (concat manual auto)))



(defn mean-points-per-user 
  "Create map of login names and id to mean points. Skips users without points."
  [x]
  (let [tasklets (xml-> x :tasklet)
        users    (map #(xml1-> % (attr :login)) tasklets)
        ids      (map #(xml1-> % (attr :taskDefId)) tasklets)
        points   (map #(stats/mean (concat (points-auto %) (points-manual %))) tasklets)]
    (dissoc-where-v #(Float/isNaN %) (zipmap (map list users ids) points))))

;;;;;;;;;;;;;;;; taskdef xmls ;;;;;;;;;;;;;;;;;;;;;

(defn categories [x]
  (xml-> x :category))

(defn cat-ids [x]
  (set (for [cat (categories x)]
         (xml1-> cat (attr :id)))))


(def subtaskdef-types #{ :mcSubTaskDef
                         :mappingSubTaskDef
                         :clozeSubTaskDef
                         :textSubTaskDef
                         :paintSubTaskDef})

(defn- find-subtaskdefs [x]
  (xml-> x :category zf/children zf/children (where-one-of? subtaskdef-types)))

(defn subtaskdefs 
  "Extract all relevant subtaskdef informations."
  [x]
  (for [s (find-subtaskdefs x)]
      (hash-map 
        :type (:tag (zip/node s))
        :id   (attr s :id)
        :points (xml1-> s zip/leftmost (attr :pointsPerTask))
        :trash? (Boolean/valueOf (attr s :trash)))))

(defn task-text 
  "Find the problem text of subtaskdef with given id."
  [x id]
  (some 
    #(xml1-> % [(attr= :id id)] zf/children :problem zfx/text)
    (find-subtaskdefs x)))

;;;;;;;;;;;;;; individual tasklet files
(def subtasklet-types #{:mcSubTask :mappingSubTask :clozeSubTask :textSubTask :paintSubTask} )
  
(defn- find-subtasklets [x]
  (xml-> x :try :page zf/children (where-one-of? subtasklet-types)))

(defn subtasklets 
  [x]
    (set
      (for [s (find-subtasklets x)]
        (hash-map 
          :type (:tag (zip/node s))
          :id   (attr s :refId)
          :virtualNum (attr s :virtualNum)
          :page (xml1-> s zip/up (attr :no))
          :manual-points (xml-> s zf/children :manualCorrection (attr :points))
          :auto-points (xml-> s zf/children :autoCorrection (attr :points))))))


(comment
  
  (def x (load-xml "D:/temp/e/ExamServerRepository_bildungssystemPruef/system/taskhandling.xml "))
  (taskdef-ids x)
  (correctors x)
  (points-of-corrector x "allpaed")
  (into {} (filter (fn [[k v]] (when (contains? #{"allpaed" "schulpaed" "vglpaed" "wollersheim"} k) [k v])) (points-per-corrector x)))

  (def td (load-xml "d:/temp/e/ExamServerRepository_bildungssystemPruef/taskdefs/klausur_bergner_21.xml"))
  (def t (load-xml "input/ExamServerRepository_bildungssystemPruef/home/1013756/complextask_10.xml"))
  )

    