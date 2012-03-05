(ns elatexam.logs.taskdef
  (:use 
    [clojure.data.zip.xml :only (xml-> xml1-> attr attr=)]
    [clojure.java.io :only (file)]
    [org.clojars.smee 
     [file :only (find-files)]
     [map :only (dissoc-where-v)]
     [util :only (s2i s2f s2l)]]
    [elatexam.logs.xml :only (load-xml where-one-of?)])
    (:require
      [clojure.zip :as zip]
      [clojure.data.zip :as zf]
      [clojure.data.zip.xml :as zfx]
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

;;;;;;;;;;;;;;;; system/taskdefs.xml ;;;;;;;;;;;;;;;;;;;;;
(defn load-taskdef-summaries [filename]
  (let [x (load-xml filename)
        tds (xml-> x :taskDef)]
    (for [t tds]
      {:title (xml1-> t (attr :title))
       :id    (xml1-> t (attr :id))
       :file  (xml1-> t :complexTaskDef (attr :complexTaskFile))})))

;;;;;;;;;;;;;;;; individual taskdef xmls ;;;;;;;;;;;;;;;;;;;;;

(defn categories [x]
  (xml-> x :category))

(defn cat-ids [x]
  (set (for [cat (categories x)]
         (xml1-> cat (attr :id)))))

(def ^{:doc "I18n of subtask types to german names"} subtaskdef-ger 
  {:mcSubTaskDef "Multiple Choice" 
   :mcSubTask "Multiple Choice" 
   :mappingSubTaskDef "Zuordnung" 
   :mappingSubTask "Zuordnung" 
   :clozeSubTaskDef  "Lückentext"
   :clozeSubTask  "Lückentext"
   :textSubTaskDef "Freitext"
   :textSubTask  "Freitext"
   :paintSubTaskDef "Zeichnen"
   :paintSubTask "Zeichnen"}
  )

(def ^{:doc "Tag names of subtask definitions."} subtaskdef-types 
  #{ :mcSubTaskDef :mappingSubTaskDef :clozeSubTaskDef :textSubTaskDef :paintSubTaskDef})

(def ^{:doc "Tag names of task block types."} taskblock-types 
  #{:mcTaskBlock :mappingTaskBlock :clozeTaskBlock :textTaskBlock :paintTaskBlock})

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
        :points (xml1-> s zip/leftmost (attr :pointsPerTask) s2f)
        :text-hash (xml1-> s zf/children :problem zfx/text hash)))))

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
        duration  (xml1-> x :config :time zfx/text s2i)
        extension (xml1-> x :config zf/children :kindnessExtensionTime zfx/text s2i)]
    (hash-map 
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
(def ^{:doc "Valid tag names of subtasklets."} subtasklet-types 
  #{:mcSubTask :mappingSubTask :clozeSubTask :textSubTask :paintSubTask})
  
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
      (for [f (find-files dir regex) :when (< 0 (.length f))]
        (let [[_ id] (re-matches regex (.getName f))
              zipper (load-xml f)]
          (hash-map 
            :taskdef-id (s2i id)
            :start-time (xml1-> zipper :try (attr :startTime) s2l)
            :random-seed (xml1-> zipper :try (attr :randomSeed) s2l)
            :user (.getName (.getParentFile f)) 
            :subtasklets (subtasklets zipper)))))))
  
  

(comment
  
  (def x (load-xml "input/system/taskhandling.xml"))
  (taskdef-ids x)
  (correctors x)
  (points-of-corrector x "allpaed")
  (into {} (filter (fn [[k v]] (when (contains? #{"allpaed" "schulpaed" "vglpaed" "wollersheim"} k) [k v])) (points-per-corrector x)))
  (def t (load-xml "input/home/1013756/complextask_10.xml"))

  (def td (subtaskdefs (load-xml "input/taskdefs/klausur_bergner_21.xml")))
  (def tries (load-tries "input/home" "12"))
  
  (def x
    (for [{f :file :as m} (load-taskdef-summaries "input/system/taskdefs.xml")]
      (merge m (load-taskdef (file "input/taskdefs" f)))))
  )

    
