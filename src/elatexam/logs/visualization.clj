(ns elatexam.logs.visualization
  (:use 
    [clojure.java.io :only (file)]
    [incanter core charts stats]
    [org.clojars.smee 
     [map :only (map-values)]
     [time :only (time-to-string time-to-date-string)]
     [util :only (s2i)]]
    [elatexam.logs.util :only (percent index-filter)])
  (:require
    [elatexam.logs 
     [core :as c]
     [stats :as stats]
     [taskdef :as td]
     [xml :as xml]])
  (:import
    [org.jfree.data.gantt Task TaskSeries TaskSeriesCollection]
    org.jfree.data.time.SimpleTimePeriod))

(defn add-domain-marker [chart x label]
  (.addDomainMarker (.getPlot chart) 
    (doto (org.jfree.chart.plot.ValueMarker. x) 
      (.setLabel label) 
      (.setPaint java.awt.Color/RED))))

(defn add-value-marker [chart x label]
  (.addRangeMarker (.getPlot chart) 
    (doto (org.jfree.chart.plot.ValueMarker. x) 
      (.setLabel label) 
      (.setPaint java.awt.Color/RED))))


(defn page-edit-graph-chart
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


(defn show-corrector-points-box-chart 
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
      plot)))


(defn exam-duration-graph-chart
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
    (use-relative-time-axis)))

(defn duration-vs-points-chart [entries th]
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

(defn exam-groups-gantt-chart 
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

(defn task-difficulty-chart 
  "Plot bar chart representing the subtask def difficulties."
  [subtaskdefs tries series-label]
  (let [difficulty (stats/task-difficulty subtaskdefs tries)
        values (remove stats/nan? (vals difficulty))]
    (doto (histogram values 
            :nbins 30
            :legend true
            :x-label "Aufgabenschwierigkeit"
            :y-label "Anzahl"
            :series-label series-label
            :title "Verteilung der Aufgabenschwierigkeit")
      (add-domain-marker 0.4 "schwer")
      (add-domain-marker 0.8 "leicht"))))


(defn- difficulties-label [subtaskdefs tries]
  (let [d         (stats/task-difficulty subtaskdefs tries)
        clusters  (stats/cluster-difficulties d)
        counted   (map-values count clusters)]
    (str "Leicht: " (counted :easy) \newline
      "Mittel: " (counted :medium)\newline
      "Schwer: " (counted :hard))))

(defn show-task-difficulties
  "Splits tries into individual groups via :random-seed, shows task-difficulty plot for
every group where size>1."
  ([subtaskdefs tries] 
    (show-task-difficulties 
      subtaskdefs tries (time-to-string (apply min (map :start-time tries)))))
  ([subtaskdefs tries label]
    (doto (task-difficulty-chart subtaskdefs tries label)
      (add-subtitle (difficulties-label subtaskdefs tries))
      (set-x-range 0 1)
      view)))

(defn show-difficulty-points-effect 
  "Show plot of points reached to number of questions of given difficulty.
hardness may be one of :easy, :medium, :hard"
  [subtaskdefs tries hardness]
    (let [students (stats/difficulty-stats subtaskdefs tries)
        x (map hardness students)
        y (map :points students)
        lm (linear-model y x)
        lm-label (str "Trend (OLS Regression)" \newline "erklärt " (percent (:adj-r-square lm)))]
    (doto (scatter-plot x y 
            :title "Zusammenhang Schwierigkeit/Punkte"
            :x-label (str "Anzahl von Fragen mit Schwierigkeit " hardness)
            :y-label "Erreichte Punkte"
            :legend true
            :series-label "Punkte")
      (add-lines x (:fitted lm) :series-label lm-label)
      (add-subtitle (str "Korrelation = " (percent (correlation x y))))
      view)
    #_lm))

(defn show-task-difficulties-per-type
  "Show one histogram of task difficulites per task type."
  [subtaskdefs tries]
  (let [types (group-by :type subtaskdefs)]
    (doseq [[t stds] types]
      (show-task-difficulties stds tries (td/subtaskdef-ger t)))))

(defn discrimination-power-charts
  "Plot item scores vs. adjusted exam scores per subtask. Print discrimination
power and difficulty as subtitle."
  ([subtaskdefs tries] (discrimination-power-charts subtaskdefs tries nil))
  ([subtaskdefs tries flt]
  (let [x (stats/discrimination-power-scores subtaskdefs tries)
        d (stats/task-difficulty subtaskdefs tries)]
    
    (into {}
      (for [[id [x y]] x :when (or flt (flt id))]
        (let [pearson (correlation x y)
              spearman (spearmans-rho x y)] 
          [id 
           (doto (scatter-plot x y :title id :x-label "Punkte in Aufgabe" :y-label "Punkte in Restprüfung")
             (add-subtitle (str "Trennschärfe: " 
                             (percent pearson) "(Pearson), " 
                             (percent spearman) "(Spearman)" \newline 
                             "Schwierigkeit: " (percent (d id)))))]))))))

(defn score-box-plot-chart 
  "Splits tries into groups of students with the same questions, renders
a box plot of exam score distributions."
  [& tries]
  (let [tries (sort-by (comp :start-time first) tries)
        starts (map #(time-to-string (apply min (map :start-time %))) tries)
        [[lbl g1] & groups] (map vector starts (map #(vals (stats/exam-scores %)) tries))  
        plot (box-plot g1 
               :legend true 
               :title "Punkte pro Gruppe" 
               :y-label "Punkte" 
               :series-label (format "%s \nmean %.2f" lbl (mean g1)))]
    (doseq [[lbl g] groups]
        (add-box-plot plot g 
          :series-label (format "%s \nmean %.2f\n(p-Wert t-test): %f" 
                          lbl
                          (mean g)
                          (:p-value (t-test g1 :y g :alternative :greater)))))
    plot))


(defn cronbach-histogram-chart 
  [subtaskdefs tries]
  (let [cb (stats/cronbach-alpha subtaskdefs tries)
        c-i-d (stats/cronbach-if-deleted subtaskdefs tries)]
    (doto (histogram (vals c-i-d) 
            :title "Cronbach's alpha if deleted"
            :x-label "Cronbach's alpha"
            :y-label "Häufigkeit")
      (add-domain-marker cb "Cronbach's alpha Gesamt")
      #_(set-x-range 0 1))))

(defn cronbach-bar-chart 
  [subtaskdefs tries]
  (let [cb (stats/cronbach-alpha subtaskdefs tries)
        c-i-d (stats/cronbach-if-deleted subtaskdefs tries)]
    (doto(bar-chart (keys c-i-d) (vals c-i-d) 
            :title "Cronbach's alpha if deleted"
            :x-label "Aufgaben"
            :y-label "Cronbach's alpha if deleted"
            :vertical false)
      (add-value-marker cb "Cronbach's alpha Gesamt")
      (add-subtitle (format "Cronbach's alpha Gesamt: %.3f" cb)))))

(defn score-histogram-chart 
  [tries]
  (let [scores (vals (stats/exam-scores tries))
        mean (mean scores)
        sd (sd scores)]
    (doto (histogram scores :nbins 10
            :title "Puntkeverteilung"
            :x-label "Gesamtpunkte"
            :y-label "Häufigkeit")
      (add-domain-marker mean "Durchschnitt")
      (add-domain-marker (- mean sd) "-1 sd")
      (add-domain-marker (+ mean sd) "+1 sd"))))


  
(comment
  (def input-dir "d:\\temp\\ExamServerRepository_bildungssystemPruef\\")
  (def entries (c/logs-from-dir input-dir))
  (def th (xml/load-xml (file input-dir "system" "taskhandling.xml")))
  (duration-vs-points-chart entries th)
  (save (exam-duration-graph entries) "d:/bearbeitungszeit.png")
  (view (task-difficulty std tries "Alle Gruppen"))
  (show-task-difficulties-per-type std tries)
  
  (def taskdef (td/load-taskdef (file input-dir "taskdefs" "klausur_bergner_21.xml")))
  (def tries (td/load-tries (file input-dir "home") "12"))
  (def std (taskdef :subtaskdefs))
  
  
  (def groups (stats/split-by-randomseed tries))
  (stats/find-bad-cronbach std (first groups))
  )

(comment
  ;; show score distributions of exam groups for individual days
  (def g1 (first groups))
  
  (def groups-per-day (map (partial group-by #(time-to-date-string (:start-time %))) groups))
  (map #(view (apply score-box-plot-chart (vals %))) groups-per-day)
  )