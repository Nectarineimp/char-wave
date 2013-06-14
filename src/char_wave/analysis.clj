(ns char-wave.analysis)

(defn roundx
  "x= number to be rounded, n= precision, maximum precision of 10."
  [x n]
  (/ (Math/round (* (Math/pow 10.0 (if (> n 10) 10 n)) x)) (Math/pow 10.0 (if (> n 10) 10 n))))

(defn -sort-part-chars
  "Takes the input an partitions every unique byte into mini collections."
  [s]
  (partition-by identity (sort s)))

(defn -char-wave [coll]
  "Takes a mini-collection of a unique byte and creates a vector of byte and count."
  (vector (int (first coll)) (count coll)))

(defn -char-waveform
  "Creates a sequence of vectors of unique byte and count. The output is the unique waveform of the input."
  [s]
  (map -char-wave (-sort-part-chars s)))

(defn -wave-size
  "Examines the waveform for element count and produces that value to be used in normalization"
  [wave]
  (reduce (fn [a b] (+ a (second b))) 0 wave))

(defn -magnify
  [x]
  "Takes a normal x, divides by 1/10 and rounds the double to zero decimiles. This creates 201 possible bins of 0-200"
  (int (roundx (* x 25) 0)))

(defn -normalize-wf
  "Creates a normalized wave form of vectors of byte and count over total samples."
  [wave]
  (let
    [scale (-wave-size wave)]
    (map #(vector (first %) (/ (second %) scale)) wave)))

;; Wilkes' Power Magic

(defn -fill-array [tuples]
  (let [xs (double-array 256)]
    (doseq [[pos x] tuples]
      (aset-double xs pos x))
    xs))

(defn -array->tuples [^doubles xs]
  (areduce xs i ret []
           (conj ret [i (aget xs i)])))

(def -fill-tuples (comp -array->tuples -fill-array))

(def generate-waveform (comp -fill-tuples -normalize-wf -char-waveform))
;; This is the main entry point into the analysis - this takes text and performs an analysis of the distribution
;; of bytes. These 8bit wave forms can then be compared and used to generate profiles from.