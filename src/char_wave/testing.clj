;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.

(ns char-wave.testing)

(use 'char-wave.analysis)

(def bad01 (take 256 (repeatedly #(int (rand 256)))))
(def bad02 (take 256 (repeatedly #(int (rand 256)))))

(def good01 "This is some normal text! It is useful in many ways. It can be used to determine if this system is working")
(def good02 "The main point of this sentence is lost. However, this sentence recovers the train of thought by mentioning Bayes Classifier!")
(def good03 "The test-constants are not evaluated. They must be compile-time literals, and need not be quoted. If the expression is equal to a
  AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA test-constant, the corresponding result-expr is returned. A single default expression can
  follow the clauses, and its value will be returned if no clause matches. If no default expression is   provided
  and no clause matches, an IllegalArgumentException is thrown.")

;; Testing WPM

(doall (map second (generate-waveform good02)))

(doall (map second (generate-waveform good03)))
(doall (map second (generate-waveform bad01)))
(doall (map second (generate-waveform bad02)))


(doall (generate-waveform good03))

(doall (map second (generate-waveform bad01)))

(generate-waveform newlines)

(doall (filter #(cond (< 0 (second %)) true :else false) (generate-waveform good03)))

(print "hello there" newlines "well that was nice")
