(ns tp-lindenmayer-systems.core
  (:require [clojure.string :as str]))

(def RULES {:F "FF" :X "F+[[X]-X]-F[-FX]+X"})
(def AXIOM "X")
(def ANGLE 22.5)

; Vector2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn new-vector-2
  "Returns a hash-map with like: :x x :y y"
  [x y]
  {:x x :y y})

(defn get-x
  "Returns the x coordinate of the given vector2"
  [vector2]
  (get vector2 :x))

(defn get-y
  "Returns the y coordinate of the given vector2"
  [vector2]
  (get vector2 :y))

(defn add-vector-2
  "Returns the addition of the two vectors"
  [vec1 vec2]
  (new-vector-2 (+ (get-x vec1) (get-x vec2))
                (+ (get-y vec1) (get-y vec2))))

(defn norm-vector-2
  "Returns the norm of the given vector-2"
  [vec-2]
  (Math/sqrt (+ (* (get-x vec-2) (get-x vec-2)) (* (get-y vec-2) (get-y vec-2)))))

(defn rotate-vector-2
  "Returns the rotation from the origin of coordinates by the given angle of the given vector-2"
  [vec-2 angle]
  (new-vector-2 (* (norm-vector-2 vec-2) (Math/cos (Math/toRadians angle)))
                (* (norm-vector-2 vec-2) (- (Math/sin (Math/toRadians angle))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Step ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn new-step
  "Returns a new step with the given start and end"
  [start end]
  {:start start :end end})

(defn get-start-step
  "Returns the start of the given step"
  [step]
  (get step :start))

(defn get-end-step
  "Returns the end of the given step"
  [step]
  (get step :end))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Turtle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn new-turtle
  "Returns a hash-map with the key value pairs :position position :angle angle :path nil. Simulates a turtle"
  ([position angle] {:position position :angle angle})
  ([turtle] {:position (get turtle :position) :angle (get turtle :angle)}))

(defn turtle-move
  "Returns a turtle that has moved away from the given turtle the given units with the given turtle's angle"
  [turtle units]
  (let [displacement (rotate-vector-2 (new-vector-2 units 0) (get turtle :angle))
        new-position (add-vector-2 (get turtle :position) displacement)]
    (new-turtle new-position (get turtle :angle))))

(defn turtle-rotate-left
  "Returns a turtle rotated to the left the given angle from the given one"
  [turtle rotation]
  (new-turtle (get turtle :position) (- (get turtle :angle) rotation)))

(defn turtle-rotate-right
  "Returns a turtle rotated to the right the given angle from the given one"
  [turtle rotation]
  (new-turtle (get turtle :position) (+ (get turtle :angle) rotation)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  "Returns the content of the given file split by newlines"
  [file-name]
  (str/split (slurp file-name) #"\n"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Key value pairings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn key-value [key value]
  (assoc nil (keyword key) value))

(defn get-rules
  "Receives a sequence of strings formatted like KEY VALUE and returns a hash-map of the KEYs paired with the VALUEs"
  [rules]
  (reduce merge (map #(apply key-value %) (map #(str/split % #" ") rules))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn list-update-first
  "Returns a list based on the given list, but with the first element replaced with the given one"
  [list elem]
  (cons elem (drop 1 list)))

(defn _path
  ""
  [l-string turtles steps angle]
  (if (= l-string "") steps
                      (let [current-turtle (first turtles)]
                         (_path (apply str (drop 1 l-string))
                                 (case (first l-string)
                                   (\F \f) (list-update-first turtles (turtle-move current-turtle 1))
                                   \+ (list-update-first turtles (turtle-rotate-right current-turtle angle))
                                   \- (list-update-first turtles (turtle-rotate-left current-turtle angle))
                                   \[ (cons (new-turtle current-turtle) turtles )
                                   \] (drop 1 turtles)
                                   turtles)

                                 (case (first l-string)
                                   \F (conj steps (new-step (get current-turtle :position) (get (turtle-move current-turtle 1) :position)))
                                   steps)
                                angle))))


(defn path
  "Returns a list of path based on the given L-system"
  [l-string turtles angle]
  (_path l-string turtles nil angle))


(defn draw [processedSystem angle outputFile]
  )

(defn -main [input-file n output-file]
  (let [processed-file (file-processing input-file)
        angle (first processed-file)
        axiom (second processed-file)
        rules (drop 2 processed-file)]))

(assert (= "X" (system-processing RULES AXIOM 0)))
(assert (= "F+[[X]-X]-F[-FX]+X" (system-processing RULES AXIOM 1)))
(assert (= "FF+[[F+[[X]-X]-F[-FX]+X]-F+[[X]-X]-F[-FX]+X]-FF[-FFF+[[X]-X]-F[-FX]+X]+F+[[X]-X]-F[-FX]+X" (system-processing RULES AXIOM 2)))
