(ns LSystems.vector2)

(defn create
  "[]: Returns a vector2 with the coordinates (0 0)
  [x y]: Returns a vector with the coordinates (x y)"
  ([] (create 0 0))
  ([_x _y] (list _x _y)))

(defn x
  "[v]: Returns the x coordinate of the given vector
  [v n]: Returns a v based on the given v with its x coordinate set to the given one"
  ([v] (first v))
  ([v n] (create n (second v))))

(defn y
  "[v]: Returns the y coordinate of the given vectors
  [v n]: Returns a v based on the given v with its y coordinate set to the given one"
  ([v] (second v))
  ([v n] (create (x v) n)))

(defn add
  "[]: Returns a vector2 with the coordinates (0 0)
  [v]: Returns v
  [v1 v2]: Returns the addition of the two vectors
  [v1 v2 & more]: Returns the sum of the given vectors2"
  ([] (create))
  ([v] v)
  ([v1 v2] (create (+ (x v1) (x v2))
                   (+ (y v1) (y v2))))
  ([v1 v2 & more] (reduce add (add v1 v2) more)))

(defn norm
  "[]: Returns a vector2 with the coordinates (0 0)
  [v]: Returns the norm of the given vector2"
  ([] (create))
  ([v] (Math/sqrt (+ (* (x v) (x v))
                     (* (y v) (y v))))))

(defn rotate
  "[]: Returns a vector2 with the coordinates (0 0)
  [v]: Returns the given vector
  [v angle]: Returns the rotation from the origin of coordinates by the given angle of the given vector-2"
  ([] (create))
  ([v] v)
  ([v angle] (create (+ (* (Math/cos (Math/toRadians angle)) (x v)) (* (Math/sin (Math/toRadians angle)) (y v)))
                     (+ (- (* (Math/sin (Math/toRadians angle)) (x v))) (* (Math/cos (Math/toRadians angle)) (y v))))))

(defn equal?
  "[v]: Returns true because a vector2 is equal to himself :).
  [v1 v2]: Returns true if both vector2s have the same coordinates.
  In another case, false."
  ([v] true)
  ([v1 v2] (and (= (x v1) (x v2))
                (= (y v1) (y v2)))))

(defn min-vector-list
  "Returns a vector with the min x coordinate and the y min coordinate of the given vectors list"
  [lv] (create (apply min (for [v lv] (x v))) (apply min (for [v lv] (y v)))))

(defn max-vector-list
  "Returns a vector with the min x coordinate and the y min coordinate of the given vectors list"
  [lv] (create (apply max (for [v lv] (x v))) (apply max (for [v lv] (y v)))))