(ns elatexam.logs.visualization
  (:use [incanter core charts stats]
    [elatexam.logs.util :only (map-values map-keys)])
  (:require
    [elatexam.logs.taskdef :as td]
    [elatexam.logs.core :as c]))

(defn add-domain-marker [chart x label]
  (.addDomainMarker (.getPlot chart) 
    (doto (org.jfree.chart.plot.ValueMarker. x) 
      (.setLabel label) 
      (.setPaint java.awt.Color/RED))))

(defn page-edit-graph
  "show histogram of changing pages"
  [entries]
  (histogram (c/editing-stats entries) 
          :nbins 100
          :title "Bearbeitung der Seiten"
          :x-label "Anzahl Speichervorgänge"
          :y-label "Anzahl Studenten"))

(defn- use-relative-time-axis 
  "Replace domain axis by relative date/time axis."
  [chart]
  (let [plot (.getPlot chart)
        rdf  (doto (org.jfree.chart.util.RelativeDateFormat. (long 0)) 
               (.setSecondFormatter (java.text.DecimalFormat. "00"))
               (.setShowZeroDays false))
        axis (doto (org.jfree.chart.axis.DateAxis.) (.setDateFormatOverride rdf))]
    (.setDomainAxis plot axis)))


(defn show-corrector-points-box 
  "Box plot of all points given by correctors. Expects map of names to seq. of points"
  [c-p-m]
  (when-let [[[n p] & fs] (seq c-p-m)]
    (let [plot (box-plot p 
                 :series-label n 
                 :legend true 
                 :x-label "Korrektoren" 
                 :y-label "Verteilung"
                 :title "Punktevergabe pro Korrektor")]
      (doseq [[n p] fs]
        (add-box-plot plot p :series-label n))
      (view plot))))


(defn exam-duration-graph
  "show histogram of exam duration"
  [entries]
  (doto (histogram 
          (remove #(> % (* 24 60 60 1000)) (map c/exam-duration (vals (c/user-entries entries)))) 
          :nbins 100
          :title "Bearbeitungsdauer"
          :x-label "Zeit in msec"
          :y-label "Anzahl Studenten")
    (add-domain-marker (* 0.5 3600000) "30min")
    (add-domain-marker (* 1   3600000) "60min")
    (add-domain-marker (* 1.5 3600000) "90min")
    (use-relative-time-axis)
    
    ))

(defn- points [x entry]
  (let [u  (c/user entry)
        id (c/taskid entry)]
    (td/points-of-user x u id)))

(defn duration-vs-points [entries th]
  (let [user-id-points (td/mean-points-per-user th)
        user-logs   (c/exams-by-user entries); map of user to map of id to logentries
        durations    (map-values (partial map-values c/exam-duration) user-logs) ;map of user to map of id to exam duration
        duration-points (into {} (mapcat  (fn [p durs] (for [[_ d] durs] [d p])) (vals user-id-points) (vals durations)))
        ]
    
    (doto (scatter-plot (keys duration-points) (vals duration-points)
            :title "Zeit vs. erreichte Punkte"
            :x-label "Bearbeitungszeit"
            :y-label "Punkte"
            :legend true
            :series-label "Punkte pro Prüfungsdauer")      
      (use-relative-time-axis))))
    ;duration-points))

(comment
  (def entries (c/logs-from-dir "d:/temp/e"))
  (def x (td/load-xml "D:/temp/e/ExamServerRepository_bildungssystemPruef/system/taskhandling.xml"))
  (duration-vs-points entries x)
  (save (exam-duration-graph elatexam.logs.core/entries) "d:/bearbeitungszeit.png")
  )