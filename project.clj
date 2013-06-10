(defproject char-wave "0.1.0-SNAPSHOT"
  :description "These functions produce a wave form of the character distribution, allowing them to be analyzed."
  :url "http://cicayda.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.0"]]
  :main char-wave.core
  :aot [char-wave.core])
