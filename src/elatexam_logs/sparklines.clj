(ns elatexam-logs.sparklines)
; Sparkline Generator
; Copyright (c) Jonathan A Watmough. All Rights Reserved.
;
; The use and distribution terms for this software are covered by the
; Common Public License 1.0 (http://opensource.org/licenses/cpl1.0.txt).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license. Do not remove this notice.
 
(defn 
#^{ :doc "Scale a list of numeric 'values' to a max height of 'height'."
    :private true}
      scale-values ([values height]
  (let [highest (apply max (filter identity values))]
    (map #(if % (dec (- height (/ (* % (- height 3)) highest))) nil) values))))
 
(defn 
#^{:doc "Make a Win-style java.awt.image.BufferedImage of 'width' x 'height'."}
      make-buffered-image ([width height]
  (let [image-type (. java.awt.image.BufferedImage TYPE_3BYTE_BGR)]
    (new java.awt.image.BufferedImage width height image-type))))
 
(defn 
#^{:doc "Create pixel-accurate sparkline bitmap graphic from {:width :height :values :marker}. :marker=-1 for last entry."}
      make-sparkline ([{ w :width h :height v :values m :marker }]
  (let [v       (scale-values v h)
        bitmap  (make-buffered-image w h)
        g       (. bitmap (getGraphics))
        m       (if (and m (< m 0)) (+ m (count v)) m)
        step    (/ (- w 3) (- (count v) 1))]
    (do (doto g (.setColor (java.awt.Color/white))
                (.fillRect 0 0 w h)
                (.setColor (java.awt.Color/gray)))
        (dotimes  [idx  (dec (count v))]
          (let [x-pos    (inc (* idx step))
                prev-val (nth v idx)
                this-val (nth v (inc idx))]
            (when (and prev-val this-val)
                  (. g (drawLine x-pos (dec prev-val) (+ x-pos step) (dec this-val))))))
        ; Draw marker if present and return the sparkline bitmap
        (when (and m (nth v m))
            (let [bx  (* m step)
                  by  (- (nth v m) 2)]
              (doto g (.setColor (java.awt.Color/red))
                      (.fillOval bx by 2 2))))
        (. g (dispose))
        bitmap))))
 
(defn show-sparkline
  [values]
  (let [spark (make-sparkline {:width 300 
                               :height 200 
                               :values values
                               :marker 10})
          icon  (new javax.swing.ImageIcon spark)]
        (doto (new javax.swing.JFrame "Sparkline")
        (.add (new javax.swing.JLabel icon))
        (.pack)
        (.setVisible true))))
(comment
  (defn 
    #^{:doc "Test code for sparklines graphic generator."}
      test-sparklines []
    (let [spark (make-sparkline {:width 100 :height 30 
                                 :values [1 2 10 8 2 5 8 12 14 3 4 15]
                                 :marker -1
                                 })
          icon  (new javax.swing.ImageIcon spark)]
      (doto (new javax.swing.JFrame "Sparkline Test")
        (.add (new javax.swing.JLabel icon))
        (.pack)
        (.setVisible true)))) 
  
  (test-sparklines))
