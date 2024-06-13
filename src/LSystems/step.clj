(ns LSystems.step
  (:require [LSystems.vector2 :as vector2]))

(defn create
  "[]: Returns a step with (0 0) (0 0) as start and end
  [end]: Returns a step with (0 0) as start and the given end
  [start end]: Returns a step with the given start and end"
  ([] (create (vector2/create)))
  ([end] (create (vector2/create) end))
  ([start end] {:start start :end end}))

(defn start
  "[s]: Returns the start of the given step
  [s v]: Returns a step based on the given one, but with its start set to the given one"
  ([s] (get s :start))
  ([s v] (create v (get s :end))))

(defn end
  "[s]: Returns the end of the given step
  [s v]: Returns a step based on the given one, but with its end set to the given one"
  ([s] (get s :end))
  ([s v] (create (start s) v)))
