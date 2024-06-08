(ns tp-lindenmayer-systems.core
  (:require [clojure.string :as str]))

(def RULES {:F "FF" :X "F+[[X]-X]-F[-FX]+X"})
(def AXIOM "X")
(def ANGLE 22.5)

; Vector2
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
  (Math/sqrt (+ (* (get-x vec-2) (get-x vec-2)) (* (get-x vec-2) (get-x vec-2)))))

(defn rotate-vector-2
  "Returns the rotation from the origin of coordinates by the given angle of the given vector-2"
  [vec-2 angle]
  (new-vector-2 (* (norm-vector-2 vec-2) (Math/cos angle))
                (* (norm-vector-2 vec-2) (Math/sin angle))))

(defn new-step
  ""
  [start end]
  {:start start :end end})

; Turtle
(defn new-turtle
  "Returns a hash-map with the key value pairs :position position :angle angle :path nil. Simulates a turtle"
  [position angle steps]
  {:position position :angle angle () :steps nil})

(defn turtle-step
  "Returns a turtle that has moved away from the given turtle the given units with the given turtle's angle.
  The new turtle remembers from where it came."
  [turtle units]
  (let [displacement (rotate-vector-2 (new-vector-2 units 0) (turtle :angle))
        new-position (add-vector-2 (turtle :position) displacement)
        new-steps (conj (vec (turtle :steps)) (new-step (turtle :position) new-position))]
    (new-turtle new-position (turtle :angle) new-steps)))

(defn turtle-fly
  "Returns a turtle that has moved away from the given turtle the given units with the given turtle's angle.
  The new turtle doesn't remember from where it came because she has alzheimer."
  [turtle units]
  (let [displacement (rotate-vector-2 (new-vector-2 units 0) (turtle :angle))
        new-position (add-vector-2 (turtle :position) displacement)]
    (new-turtle new-position (turtle :angle) (turtle :steps))))

(defn turtle-rotate-left
  "Returns a turtle rotated to the left the given angle from the given one"
  [turtle rotation]
  (new-turtle (turtle :position)
              (- (turtle :angle) rotation)
              (turtle :steps)))

(defn turtle-rotate-right
  "Returns a turtle rotated to the right the given angle from the given one"
  [turtle rotation]
  (new-turtle (turtle :position)
              (+ (turtle :angle) rotation)
              (turtle :steps)))

;;;;;;;;;;;;




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

; Key value pairings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn key-value [key value]
  (assoc nil (keyword key) value))

(defn get-rules
  "Receives a sequence of strings formatted like KEY VALUE and returns a hash-map of the KEYs paired with the VALUEs"
  [rules]
  (reduce merge (map #(apply key-value %) (map #(str/split % #" ") rules))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn draw [processedSystem angle outputFile]
  )

(defn -main [input-file n output-file]
  (let [processed-file (file-processing input-file)
        angle (first processed-file)
        axiom (second processed-file)
        rules (drop 2 processed-file)]
    (draw (system-processing (get-rules rules) axiom n) angle output-file)))

(assert (= "X" (system-processing RULES AXIOM 0)))
(assert (= "F+[[X]-X]-F[-FX]+X" (system-processing RULES AXIOM 1)))
(assert (= "FF+[[F+[[X]-X]-F[-FX]+X]-F+[[X]-X]-F[-FX]+X]-FF[-FFF+[[X]-X]-F[-FX]+X]+F+[[X]-X]-F[-FX]+X" (system-processing RULES AXIOM 2)))
