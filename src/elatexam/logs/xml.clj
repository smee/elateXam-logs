(ns elatexam.logs.xml
  (:require
    clojure.walk
    [clojure.xml :as xml]
    [clojure.zip :as zip]
    [clojure.java.io :as io]))

(defn- trim-ns 
  "Remove substring from keyword up to the first occurance of the character :"
  [kw] 
  (let [kw-string (name kw)
        fixed-kw (if (some #{\:} kw-string) (subs kw-string (inc (.indexOf kw-string ":"))) kw-string)]
    (keyword fixed-kw)))
(defn- fix [value]
  (if (keyword? value) (trim-ns value) value))

(defn remove-xml-namespaces 
  "Ignore all xml namespace prefixes by removing them recursively from all map keys and values"
  [m]
  (let [f (fn [[k v]]  [(fix k) (fix v)])]
    (clojure.walk/postwalk 
      (fn [x] (if (map? x) (into {} (map f x)) x)) 
      m)))

(defn load-xml 
  "Parse xml, strips all namespaces! Returns a zipper of the xml structure."
  [file]
  (zip/xml-zip (remove-xml-namespaces (xml/parse (io/input-stream file)))))

(defn where-one-of? 
  "Filter that matches tag names in an xml zipper against a set of names"
  [names]
  (let [s (set names)]
    [#(-> % zip/node :tag s)]))