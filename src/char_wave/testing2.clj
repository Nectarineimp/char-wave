;; 2nd testing file

(ns char-wave.testing2
  (:use
        [char-wave.analysis]
        [clojure.java.io])
  (:import [java.io File]
           [org.apache.commons.math3.distribution NormalDistribution])
)

;(defn dir-list [path]
;  (file-seq (clojure.java.io/file path)))

;(clojure.java.io/reader (nth (dir-list "./") 4))

;  (let [buf (char-array 30)
;        rdr (clojure.java.io/reader (clojure.java.io/file "README.md"))]
;    (.read rdr buf)
;    (apply str buf))

;  (apply str (char-array 512))

(def positive-training-data (list
 (generate-waveform "This is a simple sample of some text and I am not really trying to be crazy here.!@#$%^&*()_+")
 (generate-waveform "The Right honorable Frazer is visiting London on TDY attempting to make sense of Monarchy and Civility. 1299494.44 is the number.")
 (generate-waveform "I am the vampire and you are my dark angel. Werewolves circle the camp, afraid of fire but hungering for flesh. A #53 steel blade flashes")
 (generate-waveform "Don't tell people how to do things, tell them what to do and let them surprise you with their results.")
 (generate-waveform "The leadership instinct you are born with is the backbone. You develop the funny bone and the wishbone that go with it.")
 (generate-waveform "The best executive is the one who has sense enough to pick good men to do what he wants done, and self-restraint to keep from meddling with them while they do it.")
                    ))

(def Input-Positive-Count (count positive-training-data))
(def Pure-Groups-Positive (working-groups positive-training-data))
(def positive-classifier (create-classifier Pure-Groups-Positive
                          (create-waveform-details Pure-Groups-Positive Input-Positive-Count "positive")))
positive-classifier


(def negative-training-data (list
  (generate-waveform "Identification_Information:
  Citation:
    Citation_Information:
      Originator: Chris Daly, Spatial Climate Analysis Service
      Originator:
        George Taylor, the Oregon Climate Service at Oregon State University
      Publication_Date: 200009
      Title: United States Average Annual Precipitation, 1961-1990
      Geospatial_Data_Presentation_Form: Map
      Publication_Information:
        Publication_Place: Corvallis, OR, USA
        Publisher:
          Spatial Climate Analysis Service, Oregon State University;
          USDA - NRCS National Water and Climate Center, Portland, Oregon;
          USDA - NRCS National Cartography and Geospatial Center, Fort Worth,
          Texas")
  (generate-waveform "Spatial_Domain:
    Bounding_Coordinates:
      West_Bounding_Coordinate: -124.762142
      East_Bounding_Coordinate: -66.957227
      North_Bounding_Coordinate: 49.371731
      South_Bounding_Coordinate: 24.545220
  Keywords:
    Theme:
      Theme_Keyword_Thesaurus: ISO 19115 Topic Category
      Theme_Keyword: Environment
      Theme_Keyword: Geoscientific information
      Theme_Keyword: Inland waters")
  (generate-waveform "Theme:
      Theme_Keyword_Thesaurus: None
      Theme_Keyword: Precipitation
      Theme_Keyword: Rainfall
      Theme_Keyword: Climate
      Theme_Keyword: PRISM

    Place:
      Place_Keyword_Thesaurus: None
      Place_Keyword: United States
      Place_Keyword: Alabama
      Place_Keyword: Arizona
      Place_Keyword: Arkansas
      Place_Keyword: California
      Place_Keyword: Colorado
      Place_Keyword: Connecticut
      Place_Keyword: Delaware
      Place_Keyword: District of Columbia
      Place_Keyword: Florida
      Place_Keyword: Georgia
      Place_Keyword: Idaho
      Place_Keyword: Illinois
      Place_Keyword: Indiana
      Place_Keyword: Iowa
      Place_Keyword: Kansas
      Place_Keyword: Kentucky
      Place_Keyword: Louisiana
      Place_Keyword: Maine
      Place_Keyword: Maryland
      Place_Keyword: Massachusetts
      Place_Keyword: Michigan
      Place_Keyword: Minnesota
      Place_Keyword: Mississippi
      Place_Keyword: Missouri
      Place_Keyword: Montana
      Place_Keyword: Nebraska
      Place_Keyword: Nevada
      Place_Keyword: New Hampshire
      Place_Keyword: New Jersey
      Place_Keyword: New Mexico
      Place_Keyword: New York
      Place_Keyword: North Carolina
      Place_Keyword: North Dakota
      Place_Keyword: Ohio
      Place_Keyword: Oklahoma
      Place_Keyword: Oregon
      Place_Keyword: Pennsylvania
      Place_Keyword: Rhode Island
      Place_Keyword: South Carolina
      Place_Keyword: South Dakota
      Place_Keyword: Tennessee
      Place_Keyword: Texas
      Place_Keyword: Utah
      Place_Keyword: Vermont
      Place_Keyword: Virginia
      Place_Keyword: Washington
      Place_Keyword: West Virginia
      Place_Keyword: Wisconsin
      Place_Keyword: Wyoming
  Access_Constraints: None.")
  (generate-waveform "Type_of_Source_Media: Proprietary software
      Source_Time_Period_of_Content:
        Time_Period_Information:
          Range_of_Dates/Times:
            Beginning_Date: 1994
            Ending_Date: 1998
        Source_Currentness_Reference: Publication date
      Source_Citation_Abbreviation: FILTER
      Source_Contribution:
        The Gaussian filter was used to change the resolution of raster data
        from 4 km to 2 km.  The Gaussian filter was implemented as custom
        software written in FORTRAN.  For information about Gaussian filters
        see:  Barnes, Stanley L., 1964; A Technique for Maximizing Details in
        Numerical Weather Map Analysis.  Journal of Applied Meteorology, 3, 396-
        409.")))

(def Input-Negative-Count (count negative-training-data))
(def Pure-Groups-Negative (working-groups negative-training-data))
(create-waveform-details Pure-Groups-Negative Input-Negative-Count "negative")
(def negative-classifier (create-classifier Pure-Groups-Negative
                                   (create-waveform-details Pure-Groups-Negative Input-Negative-Count "negative")))

(first negative-classifier)
(rest negative-classifier)

;;
;; Test writing and reading the classifier pair
;;

(spit "classifier.gbc" (str (pr-str positive-classifier) "\n" (pr-str negative-classifier)))
(def gbc-file (slurp "classifier.gbc"))
(def example-text "This is an example of some text I would really like to test the classifier against.")
(def sample-wave (generate-waveform example-text))

(doall (map second sample-wave))

(def not-nil? (complement nil?))
(def gbscore (fn [gauss wave]
               (cond (not-nil? (:mean (first gauss)))
                     (let [u (:mean (first gauss))
                           s (:std-dev (first gauss))
                           o (second wave)]
                       (cond (> s 0.0) (.cumulativeProbability (NormalDistribution. u s) o)
                         :else (.cumulativeProbability (NormalDistribution. u (* u 0.01)) o))
                     )
               :else 0.0)
              )
)

(let [score '(0.5 0.25)]
       (cond (> (first score) (second score)) (first score)
         :else (* -1.0 (second score))))

;; 2x mean is not enough for normal distribution to get to 99%
(def tm 1)
(def tsd 1)
(def w 1)
(* 2 (.cumulativeProbability (NormalDistribution. tm tsd) (- tm (Math/abs (- tm w)))))

(defn calculate-score [gbc-filename sample-wave]
  (with-open [rdr (reader gbc-filename)]
    (let [gbc (doall (line-seq rdr))
        pos-class (read-string (first gbc))
        pos-details (first pos-class)
        pos-gauss (rest pos-class)
        neg-class (read-string (second gbc))
        neg-details (first neg-class)
        neg-gauss (rest neg-class)]
      (map (fn [gauss wave]
             (cond (= 0.0 (:mean gauss)) 0 ;unobserved, don't calculate
                   :else (cond (= 0.0 wave) 0 ;no value, score defaults to zero
                           :else (do
                                   (println "Mean:" (str (:mean gauss)) " Std-Dev:" (str (:std-dev gauss)) "Sample: " (str wave))
                                   ;(* 2 (.cumulativeProbability (NormalDistribution. (:mean gauss) (:std-dev gauss)) (- (:mean gauss) (Math/abs (- (:mean gauss) wave)))))
                                 )
             )
           ) pos-gauss sample-wave
      )
    )
  )
)

(def result-seq (calculate-score "/home/peter/cicayda/char-wave/classifier.gbc" (doall (map second sample-wave))))
result-seq