(ns char-wave.core
  (:gen-class)
  (:use [clojure.tools.cli :only [cli]]
        [char-wave.analysis])
  (:import [java.io File]))

;; TODO: Training Mode, Sampling Mode, Scoring code

(defn dir-list [path]
  (file-seq (clojure.java.io/file path)))

(defn read-512
  "Reads upto 512 characters from a UTF8 or ASCII file and returns a string containing them."
  [file]
  (let [buf (char-array 30)
          rdr (clojure.java.io/reader file)]
      (.read rdr buf)
      (apply str buf)))

(defn create-classifier
  "Accepts 256 element lists and calculates for each column: mean, stddev, count of non-zero observations (aka o) for the final classifier."
  ; TODO make this work with Arrays of double instead per Wilkes's code.
  [training-data]

  )

(defn -train
  "This is where we injest files and try to create a Gaussian Bayes Classifier."
  [options arguments]
  (let
    [training-data (list (map #(-> %
      read-512
      generate-waveform)
       dir-list (:posdir options)))]
    (create-classifier training-data)))

(defn score [classifer filename])

;(defn -sample
;  "This is where we score input file(s) and return their best score +/-."
;  [options arguments]
;  (with-open [gbc (File. (:classifier options))]
;    (def classifier (load-gbc gbc))
;    (println (map (partial score classifer) arguments))
;))

(defn -sample [options arugments])

(defn -switch
   "inputs are added to the classifier and the gdb file is written."
  [options arguments]
  (let [filename (:classifier options) f (File. filename)]
    (cond
       (.exists f) (-sample options arguments)
       :else (-train options arguments)
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