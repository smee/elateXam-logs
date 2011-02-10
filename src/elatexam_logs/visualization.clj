(ns elatexam-logs.visualization
  (:use [incanter core charts stats]
    elatexam-logs.core))

(defn add-domain-marker [chart x label]
  (.addDomainMarker (.getPlot chart) 
    (doto (org.jfree.chart.plot.ValueMarker. x) 
      (.setLabel label) 
      (.setPaint java.awt.Color/RED))))

(defn page-edit-graph
  "show histogram of changing pages"
  [entries]
  (view (histogram (editing-stats entries) 
          :nbins 100
          :title "Bearbeitung der Seiten"
          :x-label "Anzahl Speichervorgänge"
          :y-label "Anzahl Studenten")))

(defn use-relative-time-axis 
  "Replace domain axis by relative date/time axis."
  [chart]
  (let [plot (.getPlot chart)
        rdf  (doto (org.jfree.chart.util.RelativeDateFormat. (long 0)) (.setSecondFormatter (java.text.DecimalFormat. "00")))
        axis (doto (org.jfree.chart.axis.DateAxis.) (.setDateFormatOverride rdf))]
    (.setDomainAxis plot axis)))

(defn exam-duration-graph
  "show histogram of exam duration"
  [entries]
  (doto (histogram 
          (remove #(> % (* 24 60 60 1000)) (map exam-duration (vals (user-entries entries)))) 
          :nbins 100
          :title "Bearbeitungsdauer"
          :x-label "Zeit in msec"
          :y-label "Anzahl Studenten")
    (add-domain-marker (* 0.5 3600000) "30min")
    (add-domain-marker (* 1   3600000) "60min")
    (add-domain-marker (* 1.5 3600000) "90min")
    (use-relative-time-axis)
    ;(save "d:/bearbeitungszeit.png")
    ))

