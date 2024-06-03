(ns tp-lindenmayer-systems.core
  (:require [clojure.string :as str]))

(def RULES {:F "FF" :X "F+[[X]-X]-F[-FX]+X"})
(def AXIOM "X")
(def ANGLE 22.5)

; System processing (functions that process the rules of the L-Systems) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rule
  "Returns the matching rule with the given character, if there isn´t a matching rule, returns the character"
  [rules character]
  (if (contains? rules (keyword (str character))) (rules (keyword (str character)))
                                                  character))
(defn next-string
  "Returns the result of applying the rules to every character of the string"
  [rules string]
  (apply str (map #(rule rules %) string)))

(defn system-processing
  "Returns the application of the rules to every character of the string n times"
  [rules string n]
  (if (zero? n) string
                (system-processing rules (next-string rules string) (dec n))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Files (functions to process files) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn file-processing
  "Returns the content of the given file splitted by newlines"
  [file-name]
  (str/split (slurp file-name) #"\n"))

(assert (= "X" (system-processing RULES AXIOM 0)))
(assert (= "F+[[X]-X]-F[-FX]+X" (system-processing RULES AXIOM 1)))
(assert (= "FF+[[F+[[X]-X]-F[-FX]+X]-F+[[X]-X]-F[-FX]+X]-FF[-FFF+[[X]-X]-F[-FX]+X]+F+[[X]-X]-F[-FX]+X" (system-processing RULES AXIOM 2)))
