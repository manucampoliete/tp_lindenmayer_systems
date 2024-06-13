(ns LSystems.core
  (:require [LSystems.step :as step]
            [clojure.string :as str]
            [LSystems.turtle :as turtle]
            [LSystems.vector2 :as vector2])
  (:import (java.util Locale)))

(Locale/setDefault Locale/US)

(def INITIAL_ANGLE 90)
(def UNITS 100)
(def TURN_AROUND_ANGLE 180)
(def EMPTY_STRING "")
(def BORDER_OFFSET 50)
(def NEWLINE #"\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; System processing (functions that process the rules of the L-Systems) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rule
  "Returns the matching rule with the given character, if there is not a matching rule, returns the character"
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
                (recur rules (next-string rules string) (dec n))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Key value pairings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn key-value
  "Returns a hash-map with a unique key-value pair"
  [key value]
  (assoc nil (keyword key) value))

(defn get-rules
  "Receives a sequence of strings formatted like KEY VALUE and returns a hash-map of the KEYs paired with the VALUEs"
  [rules]
  (reduce merge (map #(apply key-value %) (map #(str/split % #" ") rules))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Path processing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn list-update-first
  "Returns a list based on the given list, but with the first element replaced with the given one"
  [list elem] (cons elem (rest list)))

(defn path
  "Returns a list of path based on the given L-system"
  ([l-string angle] (path l-string (list (turtle/create (vector2/create) INITIAL_ANGLE)) nil angle))
  ([l-string turtles steps angle]
   (if (= l-string EMPTY_STRING) (if (turtle/moved? (first turtles)) (conj steps (turtle/step (first turtles))) steps)
                       (let [current-turtle (first turtles)]
                         (recur (apply str (drop 1 l-string))
                                (case (first l-string)
                                  (\F \G) (list-update-first turtles (turtle/move current-turtle UNITS))
                                  (\f \g) (list-update-first turtles (turtle/fly current-turtle UNITS))
                                  \+ (list-update-first turtles (turtle/rotate-right current-turtle angle))
                                  \- (list-update-first turtles (turtle/rotate-left current-turtle angle))
                                  \[ (cons (turtle/create current-turtle) turtles)
                                  \] (drop 1 turtles)
                                  \| (list-update-first turtles (turtle/rotate-right current-turtle TURN_AROUND_ANGLE))
                                  turtles)

                                (case (first l-string)
                                  (\+ \- \] \f \g \|) (if (turtle/moved? current-turtle) (conj steps (turtle/step current-turtle)) steps)
                                  steps)
                                angle)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; .sl -> .svg (functions to process files) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn first-line
  [v-min v-max]
  (format "<svg viewBox=\"%f %f %f %f\" xmlns=\"http://www.w3.org/2000/svg\">\n"
          (double (- (vector2/x v-min) BORDER_OFFSET))
          (double (- (vector2/y v-min) BORDER_OFFSET))
          (double (- (+ (vector2/x v-max) BORDER_OFFSET) (- (vector2/x v-min) BORDER_OFFSET)))
          (double (- (+ (vector2/y v-max) BORDER_OFFSET) (- (vector2/y v-min) BORDER_OFFSET)))))

(defn step-to-line
  "Returns a svg line based on the given step"
  [s]
  (format "<line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\" stroke-width=\"10\" stroke=\"black\"/>\n"
                       (double (vector2/x (step/start s)))
                       (double (vector2/y (step/start s)))
                       (double (vector2/x (step/end s)))
                       (double (vector2/y (step/end s)))))

(defn last-line
  "Returns the closing tag of a .svg file"
  [] "</svg>\n")

(defn draw! [steps output-file]
  "Draws the given lines into the output file.
  WARNING: the impure side of the force relies within this parentheses"
  (let [all-vectors (apply conj (for [s steps] (step/start s)) (for [s steps] (step/end s)))]
    (do
      (spit output-file (first-line (vector2/min-vector-list all-vectors)
                                    (vector2/max-vector-list all-vectors)))
      (spit output-file (apply str (map step-to-line steps)) :append true)
      (spit output-file (last-line) :append true))))

(defn file-processing!
  "Returns the content of the given file split by newlines.
  WARNING: don't dare to touch this, or you will become an impure one!"
  [file-name]
  (str/split (slurp file-name) NEWLINE))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn -main [input-file n output-file]
  (let [processed-file (file-processing! input-file)
        angle (Double/parseDouble (first processed-file))
        axiom (second processed-file)
        rules (get-rules (drop 2 processed-file))]
    (draw! (path (system-processing rules axiom (Integer/parseInt n)) angle) output-file)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;