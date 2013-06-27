(ns char-wave.util
  (:import [java.io File]))

(defn dir-list [path]
  "Gets a sequence of file names - used with posdir and negdir."
  (file-seq (clojure.java.io/file path)))

(defn read-512
  "Reads upto 512 characters from a UTF8 or ASCII file and returns a string containing them."
  [file]
  (let [buf (char-array 30)
          rdr (clojure.java.io/reader file)]
      (.read rdr buf)
      (apply str buf)))

(defn write-GBC [classifier positive-classifier negative-classifier]
  (spit classifier (pr-str (list positive-classifier negative-classifier))))

(defn read-GBC [gbc-file]
  (read-string (slurp gbc-file)))