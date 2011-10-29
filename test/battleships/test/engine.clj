(ns battleships.test.engine
  (:use [battleships.engine]
        [clojure.test]))

(defn make-test-player [name]
  (reify Player
    (get-name [this] name)
    (ship-position [this ship-key] nil)
    (next-shot [this] nil)
    (shot-result [this coord result])
    (won [this])
    (lost [this])))

(def p1 (make-test-player "player 1"))
(def p2 (make-test-player "player 2"))
