(ns battleships.demo)

;; This is an example of how to create a player to submit to the server.
;; The entire namespace is submitted to the server.
;; You submit the file using the submit function in the client namespace.


(def ship-positions {:aircraft-carrier {:square "a1" :orientation :h}
                     :battleship {:square "b1" :orientation :h}
                     :destroyer {:square "c1" :orientation :h}
                     :submarine {:square "d1" :orientation :h} 
                     :patrol-boat {:square "e1" :orientation :h}})


(defn place-ship
  "The ship is a map which represents the ship. You must return a map with the square you want to place it, and the orientation (:v is vertical, :h is horizontal)"
  [ship]
  (let [pos (ship-positions (:key ship))]
    pos))

(def rows ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J"])
(def columns (vec (range 1 11)))

(defn next-shot
  "Where do you want to attack next?"
  [context opponent-context]
  (let [next-shot (str (rows (rand-int 10))
                       (columns (rand-int 10)))]
    next-shot))
