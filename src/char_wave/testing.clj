;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.

(ns char-wave.testing
  (:use [char-wave.analysis])
  (:import [org.apache.commons.math3.distribution NormalDistribution])
)

(def bad01 (take 256 (repeatedly #(int (rand 256)))))
(def bad02 (take 256 (repeatedly #(int (rand 256)))))
(def bad03 (take 256 (repeatedly #(int (rand 256)))))

(def good01 "This is some normal text! It is useful in many ways. It can be used to determine if this system is working")
(def good02 "The main point of this sentence is lost. However, this sentence recovers the train of thought by mentioning Bayes Classifier!")
(def good03 "The test-constants are not evaluated. They must be compile-time literals, and need not be quoted. If the expression is equal to a
  AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA test-constant, the corresponding result-expr is returned. A single default expression can
  follow the clauses, and its value will be returned if no clause matches. If no default expression is   provided
  and no clause matches, an IllegalArgumentException is thrown.")

;; Testing WPM

(def wf01+ (generate-waveform good01))
(def wf02+ (generate-waveform good02))
(def wf03+ (generate-waveform good03))

(def wf01- (generate-waveform bad01))
(def wf02- (generate-waveform bad02))
(def wf03- (generate-waveform bad02))

(class wf01+)

(ffirst (list wf01+ wf02+))

;(def training (seq wf01+ wf02+ wf03+))
;(def training- (cons wf01- wf02- wf03-))

(def u-mean (mean (map second (filter #(cond (< 0 (second %)) true :else false) (generate-waveform good03)))))
(def stddev (std-dev (map second (filter #(cond (< 0 (second %)) true :else false) (generate-waveform good03)))))
stddev
u-mean
(def normd (NormalDistribution. u-mean stddev))
(.getMean normd)
(.cumulativeProbability normd u-mean)
(.cumulativeProbability normd (/ u-mean 2))


;; proposed data structure to hold: byte, confidence (= observations/total observations), mean, std-dev
{:mean (.getMean normd)
 :std-dev (.getStandardDeviation normd)
 :confidence 0.0
 :postion 25}

;(map (fn [d]
;       {:mean 0.0})
;     data)


;; proposed data structure to hold: byte, confidence (= observations/total observations), mean, std-dev
