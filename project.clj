(defproject char-wave "0.3.0-SNAPSHOT"
  :description "Used to train and use a guassian bayes classifier for the analysis of distributions of bytes in files."
  :url "http://cicayda.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories [["sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"]]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.apache.commons/commons-math3 "3.2"]
                 [org.clojure/tools.cli "0.3.0-beta1"]]
  :main char-wave.core
  :aot [char-wave.core]
  :marginalia {:javascript ["mathjax/MathJax.js"]})
