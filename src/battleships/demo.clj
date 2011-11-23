(ns battleships.demo)

;; This is an example of how to create a player to submit to the server.
;; The entire namespace is submitted to the server.
;; You submit the file using the submit function in the client namespace.

(defn- random-coord 
  "Generates a random valid coordinate."
  []
  (let [rows ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J"]
        columns (vec (range 1 11))]
    (str (rows (rand-int 10))
         (columns (rand-int 10)))))

(defn- random-orientation 
  "Generates a random valid orientation."
  []
  ([:h :v] (rand-int 2)))

(defn place-ship
  "The ship is a map which represents the ship. You must return a map with the square you want to place it, and the orientation (:v is vertical, :h is horizontal)"
  [ship]
  {:square (random-coord) :orientation (random-orientation)})

(defn next-shot
  "Where do you want to attack next?"
  [context opponent-context]
  (random-coord))
