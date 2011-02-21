(ns elatexam.logs.taskdef
  (:use 
    [clojure.contrib.zip-filter.xml :only (xml-> xml1-> attr attr=)]
    [clojure.contrib.def :only (defvar)]
    [elatexam.logs.util]
    [elatexam.logs.xml :only (load-xml where-one-of?)])
    (:require
      [clojure.set :as cs]
      [clojure.zip :as zip]
      [clojure.contrib.zip-filter :as zf]
      [clojure.contrib.zip-filter.xml :as zfx]
      [incanter.stats :as stats]))

;;;;;;;;; taskhandling.xml ;;;;;;;;;;;;;;;;;;;;;;;;
(defn taskdef-ids 
  "All taskDefIds."
  [x]
  (set (xml-> x :tasklet (attr :taskDefId)) s2i))

(defn correctors 
  "Names of all correctors"
  [x]
  (set (xml-> x :tasklet :manualCorrection (attr :corrector))))


(defn points-of-corrector 
  "All points of one corrector"
  [x c]
  (xml-> x :tasklet :manualCorrection [(attr= :corrector c)] (attr :points) s2f))

(defn points-per-corrector 
  "Map of corrector name to list of points."
  [x]
  (let [c (correctors x)
        p (map (partial points-of-corrector x) c)]
    (apply assoc {} (interleave c p))))

(defn- points-auto [tasklet]
  (xml-> tasklet :autoCorrection (attr :points) s2f))

(defn- points-manual [tasklet]
  (xml-> tasklet :manualCorrection (attr :points) s2f))

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


(defvar subtaskdef-types 
  #{ :mcSubTaskDef :mappingSubTaskDef :clozeSubTaskDef :textSubTaskDef :paintSubTaskDef}
  "Tag names of subtask definitions.")

(defvar taskblock-types 
  #{:mcTaskBlock :mappingTaskBlock :clozeTaskBlock :textTaskBlock :paintTaskBlock}
  "Tag names of task block types.")

(defn- find-subtaskdefs [x]
  (xml-> x :category zf/children zf/children (where-one-of? subtaskdef-types)))

(defn subtaskdefs 
  "Extract all relevant subtaskdef informations where trash==false"
  [x]
  (set 
    (for [s (find-subtaskdefs x) :when (not (Boolean/valueOf (attr s :trash)))]
      (hash-map 
        :type (:tag (zip/node s))
        :id   (attr s :id)
        :points (xml1-> s zip/leftmost (attr :pointsPerTask) s2f)))))

(defn- max-points 
  "Sum up all reachable points per task block."
  [x]
  (let [configs (xml-> x :category zf/children (where-one-of? taskblock-types) zf/children :config)
        ppt #(xml1-> % (attr :pointsPerTask) s2f)
        nst #(xml1-> % (attr :noOfSelectedTasks) s2i)] 
    (reduce + 
      (map *
        (map ppt configs) 
        (map nst configs)))))

(defn load-taskdef 
  "Load task definition file."
  [filename]
  (let [x         (load-xml filename)
        duration      (xml1-> x :config :time zfx/text s2i)
        extension (xml1-> x :config zf/children :kindnessExtensionTime zfx/text s2i)]
    (hash-map 
      :file filename
      :time (* 1000 60 (+ duration extension))
      :title (xml1-> x :title zfx/text)
      :max-points (max-points x)
      :subtaskdefs (subtaskdefs x))))

(defn task-text 
  "Find the problem text of subtaskdef with given id."
  [x id]
  (some 
    #(xml1-> % [(attr= :id id)] zf/children :problem zfx/text)
    (find-subtaskdefs x)))

;;;;;;;;;;;;;; individual tasklet files
(defvar subtasklet-types 
  #{:mcSubTask :mappingSubTask :clozeSubTask :textSubTask :paintSubTask} 
  "Valid tag names of subtasklets.")
  
(defn- find-subtasklets [x]
  (xml-> x :try :page zf/children (where-one-of? subtasklet-types)))

(defn subtasklets 
  [x]
    (set 
      (for [s (find-subtasklets x)]
        (hash-map 
          ;:type (:tag (zip/node s)) ;can be reconstructed by joining with taskdef
          :id   (attr s :refId)
          :virtualNum (s2i (attr s :virtualNum))
          :page (xml1-> s zip/up (attr :no) s2i)
          :manual-points (xml-> s zf/children :manualCorrection (attr :points) s2f)
          :auto-points (xml1-> s zf/children :autoCorrection (attr :points) s2f)))))

(defn load-tries 
  "Load all individual tries of students. If an id is given loads only matching tries.
Keys: id, start-time, random-seed, user, subtasklets."
  ([dir] (load-tries dir "[0-9]+")) 
  ([dir id] 
    (let [regex (re-pattern (str "complextask_(" id ").xml"))] 
      (for [f (files-in dir regex) :when (< 0 (.length f))]
        (let [[_ id] (re-matches regex (.getName f))
              zipper (load-xml f)]
          (hash-map 
            :taskdef-id (s2i id)
            :start-time (xml1-> zipper :try (attr :startTime) s2l)
            :random-seed (xml1-> zipper :try (attr :randomSeed) s2l)
            :user (.getName (.getParentFile f)) 
            :subtasklets (subtasklets zipper)))))))
  
;;;;;;;;;;;; composite infos

(defn points-reached 
  ""
  [subtasklet]
  (if-let [ap (:auto-points subtasklet)]
    ap
    (when-let [mp (:manual-points subtasklet)] (stats/mean mp))))
  
(defn task-difficulty 
  "Calculate map of taskdef ids to mean of points reached in tries."
  [taskdefs tries]
  (let [td (cs/project taskdefs [:id :points])]
    (let [j (map #(cs/join td (:subtasklets %)) tries)
          exams (map (partial group-by :id) j)
          questions (apply (partial merge-with concat) exams)]
      (map-values (comp stats/mean (partial map #(/ (points-reached %) (:points %)))) questions))))

(defn nan? [i]
  (Double/isNaN i))



(comment
  
  (def x (load-xml "input/system/taskhandling.xml"))
  (taskdef-ids x)
  (correctors x)
  (points-of-corrector x "allpaed")
  (into {} (filter (fn [[k v]] (when (contains? #{"allpaed" "schulpaed" "vglpaed" "wollersheim"} k) [k v])) (points-per-corrector x)))
  (def t (load-xml "input/home/1013756/complextask_10.xml"))

  (def td (subtaskdefs (load-xml "input/taskdefs/klausur_bergner_21.xml")))
  (def tries (load-tries "input/home" "12"))
  )

    