(ns char-wave.core
  (:gen-class)
  (:use [clojure.tools.cli :only [cli]])
  (:import [java.io File]))

;; TODO: Training Mode, Sampling Mode, Scoring code
(defn -switch
  "inputs are added to the classifier and the gdb file is written."
  [options arguments]
  (let [filename (:classifier options) f (File. filename)]
    (cond
       (.exists f) (-train options arguments)
       :else (-sample options arguments)
     )
  )

  (defn -train
    "This is where we injest files and try to create a Gaussian Bayes Classifier."
    [options arguments]

    )

  (defn -sample
    "This is where we score input file(s) and return their best score +/-."
    [options arguments]
    (def pd (File. (:posdir options)))
    (def nd (File. (:negdir options)))
    (cond-> [pd nd]))

(defn -main
  [& args]
  (let [ [options arguments banner] (cli args
       ["-c" "--classifier" "Enable training or sampling mode depending if gbc file exists." :default "generic-classifier.gbc"]
       ["-p" "--posdir" "Set directory of positive training files." :default "positive-examples"]
       ["-n" "--negdir" "Set directory of negative training files." :default "negative-examples"]
       ["-h" "--help" "Display usage." :default false :flag true])]
    (when (:help options)
      (println "Congratulations! You've just attempted to use a gaussian-bayes classifer!
               If this is your first time using this classifier here are the ground rules.
               FIRST: you need a .gbc file. If you don't have one then you need to train one!
               Provide a --posdir for positive training files and --negdir for negative
               training files. Also specify your --classifier file path and name. It does not
               need to end in gbc but it would be nice if it did.

               Once the classifier is built then you can test files for their score. The score
               will be either + or - 0-1. A negative score says the file is not of the class
               while a positive score says the file is deemed in the class.

               -c myclass.gbc file1.txt file2.txt file3.txt will return a set of three scores.
               ")
      (println banner)
      (System/exit 0))
    (-switch options arguments)
 )
