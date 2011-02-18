(ns elatexam.logs.visualization
  (:use [incanter core charts stats]
    [elatexam.logs.util :only (map-values map-keys s2i time-to-string)])
  (:require
    [elatexam.logs.taskdef :as td]
    [elatexam.logs.core :as c]
    [elatexam.logs.xml :as xml])
  (:import
    [org.jfree.data.gantt Task TaskSeries TaskSeriesCollection]
    org.jfree.data.time.SimpleTimePeriod))

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

(defn duration-vs-points [entries th]
  (let [user-id-points  (td/mean-points-per-user th)
        ;map of user to map of id to exam duration
        user-id-dur     (c/exams-by-user entries c/exam-duration)
        duration-points (into {} (map (fn [[[user id] points]] [(get-in user-id-dur [user id]) points]) user-id-points))
        dp (dissoc duration-points nil)
        x (keys dp)
        y (vals dp)
        lm (linear-model y x)]
    
    (doto (scatter-plot x y
            :title "Zeit vs. erreichte Punkte"
            :x-label "Bearbeitungszeit"
            :y-label "Punkte"
            :legend true
            :series-label "Punkte pro Prüfungsdauer")      
      (use-relative-time-axis)
      (add-lines x (:fitted lm) :series-label "Trend (OLS Regression)"))))



;;;;;;;;;;; Show exam groups as gantt chart
(def ^java.text.SimpleDateFormat dateformat (java.text.SimpleDateFormat. "dd.MM HH:mm"))
(defn- exam-group-label [start end]
  (.format dateformat (java.util.Date. start)))

(defn- gantt-dataset [intervals]
  (let [series (TaskSeries. "Gruppen")
        interval-groups (group-by (comp c/semester-of first) intervals)]
    (doseq [[label intervals] interval-groups]
      (.add series
        ;; each semester is one gantt task
        (let [task (Task. label (SimpleTimePeriod. (ffirst intervals) (last (last intervals))))]
          ;; add individual exam groups as subtasks to current semester
          (doseq [[start end] intervals]
            (.addSubtask task (Task. (exam-group-label start end) (SimpleTimePeriod. start end))))
          task)))
    (doto (TaskSeriesCollection.)
      (.add series))))

(defn exam-groups-gantt 
  "Show gantt chart of exams."
  [entries]
  (let [runs (c/group-by-runs entries)
        intervals (sort-by first (keys runs))
        dataset (gantt-dataset intervals)]
    (org.jfree.chart.ChartFactory/createGanttChart
      "Prüfungsgruppen"
      "Start"
      "Datum"
      dataset
      true
      true
      false)))

(comment
  (def entries (c/logs-from-dir "d:/temp/e"))
  (def th (xml/load-xml "D:/temp/e/ExamServerRepository_bildungssystemPruef/system/taskhandling.xml"))
  (duration-vs-points entries th)
  (save (exam-duration-graph entries) "d:/bearbeitungszeit.png")
  )