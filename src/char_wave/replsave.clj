;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.

(require '[char-wave.util :as util]
         '[char-wave.analysis :as analysis])
(+ 2 2)

(def gbc (util/read-GBC "/home/peter/tymbal.gbc"))

(def posclass (first gbc))
(def negclass (second gbc))

(def posclassdetails (first posclass))
(def posclassgauss (second posclass))
(def gen-wave-from-filename (comp analysis/generate-waveform util/read-512 ))
(gen-wave-from-filename "/home/peter/Documents/tympal-classifier/POS-GBCtest/file1.txt")
(def gauss '{:mean 0.7584635416666666, :std-dev 0.04444760205390178, :observations 6})
(:observations gauss)
(:mean gauss)
(:std-dev gauss)
(:input-waveforms posclassdetails)
(:features-observed posclassdetails)

(analysis/prob-sample-fits gauss (second (first (gen-wave-from-filename "/home/peter/Documents/tympal-classifier/POS-GBCtest/file1.txt"))) (:input-waveforms posclassdetails) (:features-observed posclassdetails) )
;;(analysis/calculate-score "/home/peter/tymbal.gbc"
  ;;                        (gen-wave-from-filename
    ;;                       "/home/peter/Documents/tympal-classifier/POS-GBCtest/file2.txt"))
