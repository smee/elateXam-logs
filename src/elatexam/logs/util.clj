(ns elatexam.logs.util
  (:use 
    [clojure.java.io :only (reader)])
  (:require
    [clojure.string :as string])) 


(defn index-filter [pred coll]
  (when pred
    (keep identity (map-indexed (fn [idx elt] (when (pred elt) idx)) coll))))





(defn percent [dbl]
  (.format (java.text.NumberFormat/getPercentInstance) dbl))