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
 (generate-waveform "I am the vampire and you are my dark angel. Werewolves circle the camp, afraid of fire but hungering for flesh. A #53 steel blade flashes")))

(def training-values (map (partial map second) training-data))
training-values
(doall (apply map (fn [& args] args) training-values))
