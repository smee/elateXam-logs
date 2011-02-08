(ns elatexam-logs.util
  (:use [clojure.java.io :only (reader)]))

(defn read-lines
  "Like clojure.core/line-seq but opens f with reader. An encoding may be specified, too.
 Automatically closes the reader AFTER YOU CONSUME THE ENTIRE SEQUENCE."
  ([f] (read-lines f "UTF-8"))
  ([f encoding]
    (let [read-line (fn this [^java.io.BufferedReader rdr]
                      (lazy-seq
                        (if-let [line (.readLine rdr)]
                          (cons line (this rdr))
                          (.close rdr))))]
      (read-line (reader f :encoding encoding)))))