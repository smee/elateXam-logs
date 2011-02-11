(ns elatexam.logs.taskdef
  (:use 
    [clojure.contrib.zip-filter.xml :only (xml-> xml1-> attr attr=)]
    [clojure.java.io :only (input-stream)]
    [elatexam.logs.util :only (dissoc-where-v s2f)])
    (:require
      [clojure.contrib.zip-filter :as zf]
      [clojure.contrib.zip-filter.xml :as zfx]
      [clojure.zip :as zip]
      [clojure.xml :as xml]
      clojure.walk
      [incanter.stats :as stats]))

(defn- trim-ns 
  "Remove substring from keyword up to the first occurance of the character :"
  [kw] 
  (let [kw-string (name kw)
        fixed-kw (if (some #{\:} kw-string) (subs kw-string (inc (.indexOf kw-string ":"))) kw-string)]
    (keyword fixed-kw)))
(defn- fix [value]
  (if (keyword? value) (trim-ns value) value))

(defn- remove-xml-namespaces 
  "Ignore all xml namespace prefixes by removing them recursively from all map keys and values"
  [m]
  (let [f (fn [[k v]]  [(fix k) (fix v)])]
    (clojure.walk/postwalk 
      (fn [x] (if (map? x) (into {} (map f x)) x)) 
      m)))

(defn load-xml 
  "Parse xml, strips all namespaces! Returns a zipper of the xml structure."
  [file]
  (zip/xml-zip (remove-xml-namespaces (xml/parse (input-stream file)))))
  

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

(defn points-of-user 
  "All points of a user and a specific taskdef. Returns all manual and automatic points given."
  [x user taskdef-id]
  (let [manual (xml-> x :tasklet [(attr= :taskDefId taskdef-id) (attr= :login user)] :manualCorrection (attr :points))
        auto   (xml-> x :tasklet [(attr= :taskDefId taskdef-id) (attr= :login user)] :autoCorrection (attr :points))] 
    (map s2f (concat manual auto))))

(defn- points-auto [tasklet]
  (map s2f (xml-> tasklet :autoCorrection (attr :points))))

(defn- points-manual [tasklet]
  (map s2f (xml-> tasklet :manualCorrection (attr :points))))


(defn mean-points-per-user 
  "Create map of login names and id to mean points. Skips users without points."
  [x]
  (let [tasklets (xml-> x :tasklet)
        users    (map #(xml1-> % (attr :login)) tasklets)
        ids      (map #(xml1-> % (attr :taskDefId)) tasklets)
        points   (map #(stats/mean (concat (points-auto %) (points-manual %))) tasklets)]
    (dissoc-where-v #(Float/isNaN %) (zipmap (map list users ids) points))))

(comment
  
  (def x (load-xml "D:/temp/e/ExamServerRepository_bildungssystemPruef/system/taskhandling.xml "))
  (taskdef-ids x)
  (correctors x)
  (points-of-corrector x "allpaed")
  (into {} (filter (fn [[k v]] (when (contains? #{"allpaed" "schulpaed" "vglpaed" "wollersheim"} k) [k v])) (points-per-corrector x)))

  )

    