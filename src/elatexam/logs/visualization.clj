(ns elatexam.logs.visualization
  (:use [incanter core charts stats]
    [elatexam.logs.util :only (map-values map-keys s2i time-to-string percent)])
  (:require
    [elatexam.logs.taskdef :as td]
    [elatexam.logs.core :as c]
    [elatexam.logs.xml :as xml]
    [elatexam.logs.stats :as stats])
  (:import
    [org.jfree.data.gantt Task TaskSeries TaskSeriesCollection]
    org.jfree.data.time.SimpleTimePeriod))

(defn add-domain-marker [chart x label]
  (.addDomainMarker (.getPlot chart) 
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

(defn split-groups 
  "Splits all tries into separate groups by same random-seed. Returns only groups
of size >1, that means no individual students. If the exam was run without fixed random-seeds,
the returned seq will be empty."
  [tries]
  (let [gr (vals (group-by :random-seed tries))
        groups (remove #(= 1 (count %)) gr)]
    groups))

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
  [subtaskdefs tries]
  (let [x (stats/discrimination-power-scores subtaskdefs tries)
        d (stats/task-difficulty subtaskdefs tries)]
    
    (into {}
      (for [[id [x y]] x]
        (let [pearson (correlation x y)
              spearman (spearmans-rho x y)] 
          [id 
           (doto (scatter-plot x y :title id :x-label "Punkte in Aufgabe" :y-label "Punkte in Restprüfung")
             (add-subtitle (str "Trennschärfe: " 
                             (percent pearson) "(Pearson), " 
                             (percent spearman) "(Spearman)" \newline 
                             "Schwierigkeit: " (percent (d id)))))])))))

(defn score-box-plot-chart 
  "Splits tries into groups of students with the same questions, renders
a box plot of exam score distributions."
  [tries]
  (let [groups (split-groups tries)
        [g1 & groups] (map #(vals (stats/exam-scores %)) groups)
        plot (box-plot g1 
               :legend true 
               :title "Punkte pro Gruppe" 
               :y-label "Punkte" 
               :series-label (format "durchschn. %.2f Punkte" (mean g1)))]
    (doseq [g groups]
        (add-box-plot plot g 
          :series-label (format "durchschn. %.2f Punkte \n Wahrscheinlichkeit Verteilungsgleichheit \n(p-Wert t-test): %.2f" 
                          (mean g)
                          (:p-value (t-test g1 :y g)))))
    plot))

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
  (def entries (c/logs-from-dir "d:/temp/e"))
  (def th (xml/load-xml "D:/temp/e/ExamServerRepository_bildungssystemPruef/system/taskhandling.xml"))
  (duration-vs-points entries th)
  (save (exam-duration-graph entries) "d:/bearbeitungszeit.png")
  
  (def taskdef (td/load-taskdef "input/taskdefs/klausur_bergner_21.xml"))
  (def tries (td/load-tries "input/home" "12"))
  (def std (taskdef :subtaskdefs))
  
  (view (task-difficulty taskdef tries "Alle Gruppen"))
  (show-task-difficulties-per-type std tries)
  
  (def groups (split-groups tries))

  
  )