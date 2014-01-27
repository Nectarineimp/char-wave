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
;;
;; Scoring formula:
;; $$\sum _{ n=1 }^{ n=256 }{ \quad  } \frac { CPD({ \mu  }_{ n },\quad { \sigma  }_{ n },\quad F_{ n })\quad { C }_{ n } }{ \hat { O }  }$$
;; Where n=feature (256 considered here), F is the measurement of the sample on feature n
;; C is the confidence of the feature - how many samples had that feature in training as a percent
;; O circumflex is the number of observed features in the training set. We only score where a
;; feature was observed. So zero observation measures are dropped entirely from consideration.
;;
;; *fixed at 256 in this incarnation but you may
;; adapt this to other purposes so the math is kept intact for modification


(defn prob-sample-fits
  ;prob-sample-fits takes a gaussian distribution and a sample and
  ;determines a modified CDF from it. We assume the highest match
  ;is at the mean so the normal CDF function is used but the input
  ;is transformed around the mean and the result is doubled.
  [gauss sample]
  (cond (nil? (:std-dev gauss))
        0.0
        :else
          (* 2.0 (.cumulativeProbability
            (NormalDistribution. (:mean gauss) (cond (= 0.0 (:std-dev gauss)) 0.00001 :else (:std-dev gauss)))
              (- (:mean gauss) (Math/abs (- (:mean gauss) (second sample)))))))
)

(defn count-matches [gauss-list sample-list]
  (count (filter true? (map #(and (< 0 (:observations %1)) (< 0.0 (second %2))) gauss-list sample-list))))

(defn get-sample-observations [sample-wave]
  (count (filter #(< 0.0 %) (map second sample-wave))))

(defn get-training-observations [gauss]
  (reduce + (map #(:observations %) gauss)))

(defn calculate-score [gbc-filename sample-wave]
  "Takes a GBC filename and a sample waveform (list of 256 bytes) and produces a score set. (winning score, Positive Score, Negative Score)."
  (let [
        gbc (read-GBC gbc-filename)
        ;-----------GIVEN DETAILS-------------------------------
        pos-class   (first gbc)
        pos-details (first pos-class)
        pos-gauss   (second pos-class)
        pos-inputs  (:input-waveforms pos-details)
        neg-class   (second gbc)
        neg-details (first neg-class)
        neg-gauss   (second neg-class)
        neg-inputs  (:input-waveforms neg-details)
        ;-----------DERIVED DETAILS------------------------------
        posclass-total-obs (get-training-observations pos-gauss)
        posclass-match-obs (reduce + (map #(cond (and (< 0 (:observations %1)) (< 0.0 (second %2))) (:observations %1) :else 0) pos-gauss sample-wave))
        negclass-match-obs (reduce + (map #(cond (and (< 0 (:observations %1)) (< 0.0 (second %2))) (:observations %1) :else 0) neg-gauss sample-wave))
        negclass-total-obs (get-training-observations neg-gauss)
        pos-matches (count-matches pos-gauss sample-wave)
        neg-matches (count-matches neg-gauss sample-wave)
        sample-observations (get-sample-observations sample-wave)
        pos-sample-fitness (map #(prob-sample-fits %1 %2) pos-gauss sample-wave)
        neg-sample-fitness (map #(prob-sample-fits %1 %2) neg-gauss sample-wave)
        pos-score (reduce + (map #(* (/ %1 sample-observations) (/ posclass-match-obs posclass-total-obs)) pos-sample-fitness ))
        neg-score (reduce + (map #(* (/ %1 sample-observations) (/ negclass-match-obs negclass-total-obs)) neg-sample-fitness ))
        pos-sf (reduce + pos-sample-fitness)
        neg-sf (reduce + neg-sample-fitness)
       ]
    (list "matched observations" posclass-match-obs negclass-match-obs "score" pos-score neg-score "total observations" posclass-total-obs negclass-total-obs
          "matches" pos-matches neg-matches "sample observations" sample-observations
          "sample fitness" pos-sf neg-sf)
  )
)