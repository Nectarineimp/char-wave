(ns char-wave.analysis)

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

(defn -normalize-wf
  "Creates a normalized wave form of vectors of byte and count over total samples."
  [wave]
  (let
    [scale (-wave-size wave)]
    (map #(vector (first %) (/ (second %) scale)) wave)))

(defn -reduce-waveforms [waveforms] (map (partial map second) waveforms))

(defn -lists-only [groups] (apply map (fn [& args] args) groups))

(defn -remove-unobserved [groups] (map #(filter (partial < 0) %) groups))

(defn working-groups [waveforms]
  (-> waveforms
       -reduce-waveforms
       -lists-only
       -remove-unobserved))

(defn create-waveform-details [pure-groups input-waveform-count classifier-type]
  (list { :type classifier-type :input-waveforms input-waveform-count :features 256 :features-observed (count (filter #(not (empty? %)) pure-groups)) }))

(defn create-classifier
  [pure-groups waveform-details]
  (let [feature-gauss (map
                            #(list {:mean (mean %) :std-dev (std-dev %) :observations (count %)} )
                            pure-groups)]
    (cons waveform-details feature-gauss)))


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