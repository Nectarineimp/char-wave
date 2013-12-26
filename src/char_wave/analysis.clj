(ns char-wave.analysis
  (:import [java.io File]
           [org.apache.commons.math3.distribution NormalDistribution])
  (:use [char-wave.util]
        [clojure.java.io]))

;; Math functions

(defn roundx
  "x= number to be rounded, n= precision, maximum precision of 10."
  [x n]
  (/ (Math/round (* (Math/pow 10.0 (if (> n 10) 10 n)) x)) (Math/pow 10.0 (if (> n 10) 10 n))))

(defn mean [s]
  (cond (= 0 (count s)) nil :else (/ (apply + s) (count s))))

(defn std-dev [s]
  (cond
   (= 0 (count s)) nil
  :else (let [m (mean s)
        n (count s)]
    (->> s
         (map #(- % m))
         (map #(* % %))
         mean
         Math/sqrt))))

;; Waveform Functions

(defn sort-part-chars
  "Takes the input an partitions every unique byte into mini collections."
  [s]
  (partition-by identity (sort s)))

(defn char-wave [coll]
  "Takes a mini-collection of a unique byte and creates a vector of byte and count."
  (vector (int (first coll)) (count coll)))

(defn char-waveform
  "Creates a sequence of vectors of unique byte and count. The output is the unique waveform of the input."
  [s]
  (map char-wave (sort-part-chars s)))

(defn wave-size
  "Examines the waveform for element count and produces that value to be used in normalization"
  [wave]
  (reduce (fn [a b] (+ a (second b))) 0 wave))

(defn normalize-wf
  "Creates a normalized wave form of vectors of byte and count over total samples."
  [wave]
  (let
    [scale (wave-size wave)]
    (map #(vector (first %) (/ (second %) scale)) wave)))

(defn reduce-waveforms [waveforms] (map (partial map second) waveforms))

(defn lists-only [groups] (apply map (fn [& args] args) groups))

(defn remove-unobserved [groups] (map #(filter (partial < 0) %) groups))

(defn working-groups [waveforms]
  (-> waveforms
      reduce-waveforms
      lists-only
      remove-unobserved))

(defn create-waveform-details [pure-groups input-waveform-count classifier-type]
  (hash-map :type classifier-type :input-waveforms input-waveform-count :features 256 :features-observed (count (filter #(not (empty? %)) pure-groups))))

(defn create-classifier
  [pure-groups waveform-details]
  (let [feature-gauss (map
                       (fn [x] {:mean (mean x) :std-dev (std-dev x) :observations (count x)})
                       pure-groups)
        ]
    (list waveform-details feature-gauss)))


;; Wilkes' Power Magic

(defn fill-array [tuples]
  (let [xs (double-array 256)]
    (doseq [[pos x] tuples]
      (aset-double xs pos x))
    xs))

(defn array->tuples [^doubles xs]
  (areduce xs i ret []
           (conj ret [i (aget xs i)])))

(def fill-tuples (comp array->tuples fill-array))

(def generate-waveform (comp fill-tuples normalize-wf char-waveform))

;; scoring algorithm
;; gauss is a 256 element array of mean, stddev and observations.
;; details contains the number of samples examined (files), the
;; number of features* and observed features.
;; $$\[
;; \frac{-b\pm\sqrt{b^2-4ac}}{2a}
;; \]$$
;; Scoring formula:
;; $$\sum _{ n=1 }^{ n=256 }{ \quad  } \frac { CPD({ \mu  }_{ n },\quad { \sigma  }_{ n },\quad F_{ n })\quad { C }_{ n } }{ \hat { O }  }$$
;; Where n=feature (256 considered here), F is the measurement of the sample on feature n
;; C is the confidence of the feature - how many samples had that feature in training as a percent
;; O circumflex is the number of observed features in the training set. We only score where a
;; feature was observed. So zero observation measures are dropped entirely from consideration.
;;
;; *fixed at 256 in this incarnation but you may
;; adapt this to other purposes so the math is kept intact for modification

(defn prob-sample-fits [gauss wave total-inputs features-observed]
  "Calculates the probability a sample fits into the distribution of the feature,
  with confidence based upon the number of observations of the feature, and proportion
  for the number of total observed features.

  Gauss contains the distributions for all the features, wave are the measured features
  of the sample, total-inputs is the size of the training set (used for confidence)
  and features-observed is the number of features scored for the classifier (proportion.)

  The number of observations must be greater than 1 for a feature in order to be scored."
  ;(println "\n\nGauss: " gauss " Wave: " wave "\n\n")
  (cond (or (> 2 (:observations gauss))
            (= 0.0 (:std-dev gauss))) 0
        :else
        (do
          (/
           (* (/ (:observations gauss) total-inputs) ;confidence modifier
            (* 2
               (.cumulativeProbability
                (NormalDistribution. (:mean gauss) (:std-dev gauss))
                (- (:mean gauss)
                   (Math/abs (- (:mean gauss)
                                (second wave)))))))
           features-observed) ;; only observed features contribute.
        )
  )
)

(defn calculate-score [gbc-filename sample-wave]
  "Takes a GBC filename and a sample waveform (list of 256 bytes) and produces a score set. (winning score, Positive Score, Negative Score)."

  (let [gbc (read-GBC gbc-filename)
        pos-class   (first gbc)
        pos-details (first pos-class)
        pos-gauss   (second pos-class)
        neg-class   (second gbc)
        neg-details (first neg-class)
        neg-gauss   (second neg-class)]
    (let [score (list
                 (reduce + (map #(prob-sample-fits %1 %2 %3 %4)
                                pos-gauss
                                sample-wave
                                (repeat (:input-waveforms pos-details))
                                (repeat (:features-observed pos-details))))
                 (reduce + (map #(prob-sample-fits %1 %2 %3 %4)
                                neg-gauss
                                sample-wave
                                (repeat (:input-waveforms neg-details))
                                (repeat (:features-observed neg-details))))) ]
      ;; chose the score that is the absolute largest. If the negative classifier wins, multiply by -1.0.
      ;(cond (> (first score) (second score)) (list (first score) (first score) (* -1.0 (second score)))
      ;      :else (list (* -1.0 (second score)) (first score) (* -1.0 (second score))))
      score
    )
  )
)
