(ns char-wave.core
  (:gen-class)
  (:use [clojure.tools.cli :only [cli]]
        [char-wave.analysis]
        [char-wave.util])
  (:import [java.io File]))



(defn -train
  "This is where we injest files and try to create a Gaussian Bayes Classifier."
  [options]
  (let
    [training-data-positive (list (map #(-> %
      read-512
      generate-waveform)
       dir-list (:posdir options)))
     Input-Pos-Count (count training-data-positive)
     training-data-negative (list (map #(-> %
      read-512
      generate-waveform)
       dir-list (:negdir options)))
     Input-Neg-Count (count training-data-negative)
     Pure-Groups-Positive (working-groups training-data-positive)
     Pure-Groups-Negative (working-groups training-data-negative)
     Positive-Classifier (create-classifier Pure-Groups-Positive (create-waveform-details Pure-Groups-Positive Input-Pos-Count "positive"))
     Negative-Classifier (create-classifier Pure-Groups-Negative (create-waveform-details Pure-Groups-Negative Input-Neg-Count "negative"))]
    (write-GBC (:classifier options) Positive-Classifier Negative-Classifier))
  )

(defn -sample [options arguments]
  (let
    [gbc-file (read-GBC (:classifier options))
     pos-class (nth gbc-file 0)
     neg-class (nth gbc-file 1)
     pos-details (first pos-class)
     pos-gauss (second pos-class)
     neg-details (first neg-class)
     neg-gauss (second neg-class)]
    (map
     #(let [pos-score (-score pos-details pos-gauss %)
            neg-score (-score neg-details neg-gauss %)]
        (cond (> pos-score neg-score) pos-score
              :else (* -1 neg-score))
        )
     arguments)
    )
  )

(defn -switch
   "inputs are added to the classifier and the gdb file is written."
  [options arguments]
  (let [filename (:classifier options) f (File. filename)]
    (cond
       (.exists f) (-sample options arguments)
       :else (-train options)
    )
  )
)

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

               gbc -c myclass.gbc file1.txt will return a score for file1.txt")
      (println banner))
    (-switch options arguments)
 )
)