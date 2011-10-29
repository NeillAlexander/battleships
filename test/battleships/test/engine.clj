(ns battleships.test.engine
  (:import
   [battleships.engine ShipPosition])
  (:use
   [battleships.engine]
   [clojure.test]))

(def ship-positions {:aircraft-carrier (ShipPosition. "a1" :h),
                     :battleship (ShipPosition. "b1" :h),
                     :destroyer (ShipPosition. "c1" :h)
                     :submarine (ShipPosition. "d1" :h)
                     :patrol-boat (ShipPosition. "e1" :h)})

(defn make-test-player [name]
  (reify Player
    (get-name [this] name)
    (ship-position [this ship] (ship-positions (:key ship)))
    (next-shot [this] nil)
    (shot-result [this coord result])
    (won [this])
    (lost [this])))

(def p1 (make-test-player "player 1"))
(def p2 (make-test-player "player 2"))
