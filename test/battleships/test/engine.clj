(ns battleships.test.engine
  (:use     [battleships.engine]
            [clojure.test])
  (:import  [battleships.engine ShipPosition])
  (:require [battleships.game :as game]))

(def ship-positions {:aircraft-carrier (ShipPosition. "a1" :h),
                     :battleship (ShipPosition. "b1" :h),
                     :destroyer (ShipPosition. "c1" :h)
                     :submarine (ShipPosition. "d1" :h)
                     :patrol-boat (ShipPosition. "e1" :h)})

;; failing because try to place to ships on a1
(def failing-positions {:aircraft-carrier (ShipPosition. "a1" :h),
                        :battleship (ShipPosition. "a1" :h),
                        :destroyer (ShipPosition. "c1" :h)
                        :submarine (ShipPosition. "d1" :h)
                        :patrol-boat (ShipPosition. "e1" :h)})

(defn make-test-player [name positions-map result-atom]
  (reify Player
    (get-name [this] name)
    (bot? [this] true)
    (ship-position [this ship] (positions-map (:key ship)))
    (next-shot [this] nil)
    (shot-result [this coord result])
    (you-won [this] (reset! result-atom true))
    (you-lost [this] (reset! result-atom false))))

(def p1-result-atom (atom nil))
(def p1 (make-test-player "player 1" ship-positions p1-result-atom))

(def p2-result-atom (atom nil))
(def p2 (make-test-player "player 2" ship-positions p2-result-atom))

(def stupid-player-result-atom (atom nil))
(def stupid-player (make-test-player "failer" failing-positions stupid-player-result-atom))

(deftest test-failing-bot
  (let [game (game/new-game)]
    (is (not (:failed (:player2 (place-ships game :player2 p2)))))
    (is (:failed (:player2 (place-ships game :player2 stupid-player))))))


(deftest test-play-with-failing-bot
  (let [game (game/new-game)]
    (play p1 stupid-player)
    (is (true? @p1-result-atom))
    (is (false? @stupid-player-result-atom))))
