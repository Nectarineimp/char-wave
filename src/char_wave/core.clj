(ns char-wave.core
  (:gen-class)
  (:require [clojure.tools.cli :as cli]
            [char-wave.analysis :as analysis]
            [char-wave.util :as util]
            [clojure.pprint :as pp]
            [clojure.string :as string]
  )
  (:import [java.io File])
)

(def gen-wave-from-filename (comp util/read-512 analysis/generate-waveform))

(defn train
  "This is where we injest files and try to create a Gaussian Bayes Classifier."
  [options]
  (let
    [
     ;; Positive Data Points
     training-data-positive (list (map #(-> %
      util/read-512
      analysis/generate-waveform)
       (filter #(not (.isDirectory %)) (doall (util/dir-list (:posdir options))))))
     Input-Pos-Count (count (first training-data-positive))
     Pure-Groups-Positive (analysis/working-groups (first training-data-positive))
     Positive-Classifier (analysis/create-classifier Pure-Groups-Positive (analysis/create-waveform-details Pure-Groups-Positive Input-Pos-Count "positive"))

     ;; Negative Data Points
     training-data-negative (list (map #(-> %
      util/read-512
      analysis/generate-waveform)
       (filter #(not (.isDirectory %)) (doall (util/dir-list (:negdir options))))))
     Input-Neg-Count (count (first training-data-negative))
     Pure-Groups-Negative (analysis/working-groups (first training-data-negative))
     Negative-Classifier (analysis/create-classifier Pure-Groups-Negative (analysis/create-waveform-details Pure-Groups-Negative Input-Neg-Count "negative"))
    ]
    ;; put together final paired classifier
    (util/write-GBC (:classifier options) Positive-Classifier Negative-Classifier)
  )
)

(defn sample [options arguments]
  (let [score-set (map #(list % (analysis/calculate-score (:classifier options) (analysis/generate-waveform %))) arguments)]
    (pp/pprint score-set))
)

(defn switch
   "inputs are added to the classifier and the gdb file is written."
  [options arguments]
  (let [filename (:classifier options) f (File. filename)]
    (cond
       (.exists f) (sample options arguments)
       :else (train options)
    )
  )
)

(def cli-options
 [;; First three strings describe a short-option, long-option with optional
   ;; example argument description, and a description. All three are optional
   ;; and positional.
   ["-c" "--classifier CLASSFILE" "Enable training or sampling mode depending if gbc file exists."
    :default "generic-classifier.gbc"
    :parse-fn #(string/trim %)]
   ["-p" "--posdir POSDIR" "Set directory of positive training files."
    :default "positive-examples"
    :parse-fn #(string/trim %)]
   ["-n" "--negdir NEGDIR" "Set directory of negative training files."
    :default "negative-examples"
    :parse-fn #(string/trim %)]
   ["-h" "--help" "Display usage."
    :default false
    :flag true]
   ]
)

(defn usage [options-summary]
  (->> ["Congratulations! You've just attempted to use a gaussian-bayes classifer!"
        "If this is your first time using this classifier here are the ground rules."
        "FIRST: you need a .gbc file. If you don't have one then you need to train one!"
        "Provide a --posdir for positive training files and --negdir for negative"
        "training files. Also specify your --classifier file path and name. It does not"
        "need to end in gbc but it would be nice if it did."
        ""
        "Once the classifier is built then you can test files for their score. The score"
        "will be either + or - 0-1. A negative score says the file is not of the class"
        "while a positive score says the file is deemed in the class."
        ""
        "gbc -c myclass.gbc file1.txt will return a score for file1.txt"
        "----------"
        "Options:"
        options-summary]
       (string/join \newline))
)

(defn error-msg [errors]
  (str "The following errors occured while executing your command:\n\n"
       (string/join \newline errors))
)

(defn -main
  [& args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
    ;; handel help and error conditions
    (if
      (or (:help options) (= (count args) 0))
      (println (usage summary))
      (switch options arguments)
    )
  )
)
