(ns battleships.monkey)

;; This is a chaos monkey, and implementation of the player
;; that does bad things.

(defn chaos-fn [normal-fn & args]
  (condp = (rand-nth [:normal :nil :exception :rand-number :rand-string])
    :normal (apply normal-fn args)
    :nil nil
    :exception (throw (RuntimeException. "Feed me bananas!!"))
    :rand-number (rand-int 1000)
    :rand-string (.. (java.util.UUID/randomUUID) toString)))

(defn normal-place-ship
  "The ship is a map which represents the ship. You must return a map with the square you want to place it, and the orientation (:v is vertical, :h is horizontal)"
  [ship]  
  (let [ship-positions {:aircraft-carrier {:square "a1" :orientation :h}
                        :battleship {:square "b1" :orientation :h}
                        :destroyer {:square "c1" :orientation :h}
                        :submarine {:square "d1" :orientation :h} 
                        :patrol-boat {:square "e1" :orientation :h}}
        pos (ship-positions (:key ship))]
    pos))

(defn place-ship
  "The ship is a map which represents the ship. You must return a map with the square you want to place it, and the orientation (:v is vertical, :h is horizontal)"
  [ship]
  (chaos-fn normal-place-ship ship))

(defn normal-next-shot
  "Where do you want to attack next?"
  [context opponent-context]
  (let [rows ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J"]
        columns (vec (range 1 11))
        next-shot (str (rows (rand-int 10))
                       (columns (rand-int 10)))]
    next-shot))

(defn next-shot
  "Where do you want to attack next?"
  [context opponent-context]
  (chaos-fn normal-next-shot context opponent-context))
