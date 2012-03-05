(ns elatexam.logs.convertold
  (:use 
    [clojure.data.zip.xml :only (xml-> xml1-> attr attr=)]
    elatexam.logs.util
    [elatexam.logs.xml :only (load-xml)])
  (:require
    [clojure.zip :as z]
    [clojure.data.zip :as zf]
    [clojure.data.zip.xml :as zfx]))


(defn fix-ids 
  "Assign sequential ids in this xml zipper."
  [u idcount-attr] 
  (let [numusers (count (xml-> u z/children))
        u        (z/edit u assoc-in [:attrs idcount-attr] numusers)]    
    (loop [loc (z/down u) id 1]
      (let [changed (z/edit loc assoc-in [:attrs :id] id)
            next    (z/right changed)]
          (if (nil? next)
            (z/root changed)
            (recur next (inc id)))))))

(defn combine-children [u1 u2 unique-fn]
  (let [known  (set (map unique-fn (z/children u1)))
        to-add (remove (comp known unique-fn) (z/children u2))]
    (reduce #(z/append-child %1 %2) u1 to-add)))

(comment
  (let [u1 (load-xml "D:/__fix-examdata/ss08-ws1011/system/users.xml")
        u2 (load-xml "D:/__fix-examdata/ws0708/system/users.xml")
        c (combine-children u1 u2 (comp :username :attrs))
        x (with-out-str (clojure.xml/emit (fix-ids c :IdCount)))]
    (spit "d:/__fix-examdata/test4.xml" x))
  
  (let [t1 (load-xml "D:/__fix-examdata/ss08-ws1011/system/taskhandling.xml")
        t2 (load-xml "D:/__fix-examdata/ws0708/system/taskhandling.xml")
        c (combine-children t1 t2 (comp :taskDefId :attrs))
        x (with-out-str (clojure.xml/emit (fix-ids c :idCount)))]
    (spit "d:/__fix-examdata/test5.xml" x))
  
  

  
  (spit "d:/__fix-examdata/test.xml" (with-out-str (clojure.xml/emit (combine-users u1 u2))))
  )