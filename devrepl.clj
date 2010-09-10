(ns #^{:doc "A sample development.environment Clojure script"
       :author "Daniel Solano Gómez"}
  example-repl
  (:import vimclojure.nailgun.NGServer
           java.net.InetAddress)
  (:use #_clojure.contrib.repl-utils
          clojure.test))

(defn- dotests 
  "Reload all namespaces the tests depend on and run all tests"
  []
  (require :reload-all :verbose 'foobar.test.core)
  (run-tests 'foobar.test.core))
  
(def ng-host "127.0.0.1")
(def ng-port 2113)
(def ng-server (NGServer. (InetAddress/getByName ng-host) ng-port))
(.start (Thread. ng-server))

(println "Welcome to your development REPL")
(clojure.main/repl)
(.shutdown ng-server false)