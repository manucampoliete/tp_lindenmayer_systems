(ns LSystems.turtle
  (:require [LSystems.vector2 :as vector2])
  (:require [LSystems.step :as step]))

(defn create
  "[position angle]: Returns a turtle with the given position as its current position and its previous position, and the given angle

  [position angle previous-position]: Returns a turtle with the given position, angle and previous position

  [t]: Returns a turtle with the given turtle's position as its position and previous position, and the given one's angle as its angle"

  ([position angle previous-position] {:position position
                                       :angle angle
                                       :previous-position previous-position})

  ([position angle] (create position angle position))

  ([t] (create (get t :position)
               (get t :angle))))

(defn position
  "Returns the position of the given turtle"
  [t] (get t :position))

(defn angle
  "Returns the angle of the given turtle"
  [t] (get t :angle))

(defn previous-position
  "Returns the previous position of the given turtle"
  [t] (get t :previous-position))

(defn move
  "Returns a turtle that has moved away from the given turtle n units in the direction of the turtle's angle,
  it doesn't update the previous position"
  ([t n] (create (vector2/add (position t) (vector2/rotate (vector2/create n 0) (angle t)))
                 (angle t)
                 (previous-position t))))

(defn fly
  "Returns a turtle that has moved away from the given turtle n units in the direction of the turtle's angle,
  it updates the previous position because the turtle can't breathe well while flying and gets disoriented"
  ([t n] (create (vector2/add (position t) (vector2/rotate (vector2/create n 0) (angle t)))
                 (angle t)
                 (vector2/add (position t) (vector2/rotate (vector2/create n 0) (angle t))))))

(defn rotate-left
  "Returns a turtle rotated to the left the given angle from the given turtle, it updates the previous position to the current position"
  [t a] (create (position t) (+ (angle t) a)))

(defn rotate-right
  "Returns a turtle rotated to the right the given angle from the given turtle, it updates the previous position to the current position"
  [t a] (create (position t) (- (angle t) a)))

(defn step
  "Returns the current step of the turtle (a step with its start on the previous position and its end on the current position"
  [t] (step/create (previous-position t) (position t)))

(defn moved?
  "Returns true if the given turtle's position differs from its previous position"
  [t] (not (vector2/equal? (position t) (previous-position t))))
