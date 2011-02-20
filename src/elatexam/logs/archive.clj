(ns elatexam.logs.archive
  "Functions to help processing zip archive contents."
  (:use
    [clojure.contrib.io :only (to-byte-array)])
  (:import
    [java.io File ByteArrayInputStream]
    [java.util.zip ZipInputStream ZipEntry ZipFile]))

(defn extract-entry
  "Extract file from zip, returns byte[]."
  [zipfile filename]
  (with-open [zf (ZipFile. zipfile)]
    (if-let [entry (.getEntry zf filename)]
      (to-byte-array (.getInputStream zf entry)))))

(defn- filter-entries [zf regex]
  (filter #(re-matches regex (.getName %))
            (enumeration-seq (.entries zf))))

(defn get-entries
  "Sequence of name of all entries of a zip archive matching a regular expression."
  ([^File zipfile] (get-entries zipfile #".*"))
  ([^File zipfile regex] 
    (with-open [zf (ZipFile. zipfile)]
      (doall (map (fn [^ZipEntry ze] (.getName ze)) (filter-entries zf regex))))))

(defn process-entries
  "Run function for every entry (two parameters: entry name and contents as byte-array), 
returns sequence of results (not lazy)."
  ([zipfile func] (process-entries zipfile func #".*"))
  ([zipfile func regex]
    (with-open [zf (ZipFile. zipfile)]
      (doall 
        (map #(func (.getName %) (to-byte-array (.getInputStream zf %))) (filter-entries zf regex))))))  
      
      
(comment
  ;; map of entry name to binary length for all xmls files in the archive, that have the substring "1234" in their name
  (apply merge (process-entries "test.zip" #(hash-map %1 (count %2)) #".*1234.*xml"))
  )