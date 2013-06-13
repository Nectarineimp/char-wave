(ns char-wave.core
  (:gen-class))

(defn -main
  "Take stdin, test for semantic text, return nil if none found or all input if it is ok."
  [& args]
  ;(map (fn [filename] (with-open [rdr (clojure.java.io/reader filename)]) (char-wave.analysis/char-map-freq rdr)))
  )

