(defproject elatexam-logs "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/data.zip "0.1.0"]
                 [org.clojars.smee/common "1.2.0-SNAPSHOT"]
                 [org.clojars.smee/archive "0.2.0-SNAPSHOT"]
                 [chart-utils "1.0.0-SNAPSHOT"]
                 [incanter "1.3.0-SNAPSHOT" 
                  :exclusions 
                  [swank-clojure swingrepl
                   incanter/incanter-excel 
                   incanter/incanter-latex 
                   incanter/incanter-pdf 
                   incanter/incanter-mongodb 
                   incanter/incanter-processing
                   jline]]])
