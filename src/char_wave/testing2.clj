;; 2nd testing file

(ns char-wave.testing2
  (:use
        [char-wave.analysis])
  (:import [java.io File]))

;(defn dir-list [path]
;  (file-seq (clojure.java.io/file path)))

;(clojure.java.io/reader (nth (dir-list "./") 4))

;  (let [buf (char-array 30)
;        rdr (clojure.java.io/reader (clojure.java.io/file "README.md"))]
;    (.read rdr buf)
;    (apply str buf))

;  (apply str (char-array 512))

(def training-data (list
 (generate-waveform "This is a simple sample of some text and I am not really trying to be crazy here.!@#$%^&*()_+")
 (generate-waveform "The Right honorable Frazer is visiting London on TDY attempting to make sense of Monarchy and Civility. 1299494.44 is the number.")
 (generate-waveform "I am the vampire and you are my dark angel. Werewolves circle the camp, afraid of fire but hungering for flesh. A #53 steel blade flashes")
 (generate-waveform "Don't tell people how to do things, tell them what to do and let them surprise you with their results.")
 (generate-waveform "The leadership instinct you are born with is the backbone. You develop the funny bone and the wishbone that go with it.")
 (generate-waveform "The best executive is the one who has sense enough to pick good men to do what he wants done, and self-restraint to keep from meddling with them while they do it.")
                    ))

(def Input-Positive-Count (count training-data))

(def Pure-Groups-Positive (working-groups training-data))

(create-waveform-details Pure-Groups-Positive Input-Positive-Count "positive")

(def classifier (create-classifier Pure-Groups-Positive
                                   (create-waveform-details Pure-Groups-Positive Input-Positive-Count "positive")))

(first classifier)

(rest classifier)

(clojure.pprint/pprint classifier)

(spit "spit.txt" (pr-str classifier))

(def read-classifer (read-string (slurp "spit.txt")))

(:type (ffirst read-classifer))
(first (second read-classifer))

(defn meanfreq [a]
  (:mean (first a)))

(defn stddevfreq [a]
  (:std-dev (first a)))

(def meanlist (map meanfreq (rest read-classifer)))
(def stddevlist (map stddevfreq (rest read-classifer)))
