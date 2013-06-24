(ns char-wave.core
  (:gen-class)
  (:use [clojure.tools.cli :only [cli]]
        [char-wave.analysis])
  (:import [java.io File]))

(defn dir-list [path]
  "Gets a sequence of file names - used with posdir and negdir."
  (file-seq (clojure.java.io/file path)))

(defn read-512
  "Reads upto 512 characters from a UTF8 or ASCII file and returns a string containing them."
  [file]
  (let [buf (char-array 30)
          rdr (clojure.java.io/reader file)]
      (.read rdr buf)
      (apply str buf)))

(defn write-GBC [classifier positive-classifier negative-classifier]
  (spit classifier (pr-str (list positive-classifier negative-classifier))))

(defn read-GBC [gbc-file]
  (read-string (slurp gbc-file)))

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
    (Write-GBC (:classifier options) Positive-Classifier Negative-Classifier))
  )

(defn -score [classifer filename])

(defn -sample [options arugments]
  (let
    [gbc-file (read-GBC (:classifier options))
     pos-class (nth gbc-file 0)
     neg-class (nth gbc-file 1)
     pos-details (first pos-class)
     pos-gauss (second pos-class)
     neg-details (first neg-class)
     neg-gauss (second neg-class)]))

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