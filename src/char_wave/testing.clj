;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.

(ns char-wave.testing)

(use 'char-wave.analysis)

(def bad01 (take 100 (repeatedly #(int (rand 256)))))
(def bad02 (take 100 (repeatedly #(int (rand 256)))))

(def good01 "This is some normal text! It is useful in many ways. It can be used to determine if this system is working")
(def good02 "The main point of this sentence is lost. However, this sentence recovers the train of thought by mentioning Bayes Classifier!")

(def bad01wf (char-wave.analysis/char-waveform bad01))
(def bad02wf (char-wave.analysis/char-waveform bad02))

(def good01wf (char-wave.analysis/char-waveform good01))
(def good02wf (char-wave.analysis/char-waveform good02))

;; Wilkes' Power Magic

(defn -fill-array [tuples]
  (let [xs (int-array 256)]
    (doseq [[pos x] tuples]
      (aset-int xs pos x))
    xs))

(defn -array->tuples [^ints xs]
  (areduce ^ints xs i ret []
           (conj ret [i (aget xs i)])))

(def -fill-tuples (comp array->tuples fill-array))
;; Testing WPM

(fill-tuples (-normalize-wf good01wf))
(def generate-waveform (comp -fill-tuples -normalize-wf -char-waveform))

(generate-waveform good01)