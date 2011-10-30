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
  (let [num-shots (atom 0)
        shots ["a1" "a2" "a3" "a4" "a5" "a6" "a11"]] ;; first 5 sink ac, next misses, last fails
    (reify Player
      (get-name [this] name)
      (bot? [this] true)
      (ship-position [this ship] (positions-map (:key ship)))
      (next-shot [this]
        (let [shot (nth shots @num-shots)]
          (if (< @num-shots (dec (count shots)))
            (swap! num-shots inc))          
          shot))
      (shot-result [this coord result])
      (you-won [this]
        (println (str name " won!"))
        (reset! result-atom true))
      (you-lost [this]
        (println (str name " lost!"))
        (reset! result-atom false)))))

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

(deftest test-fire-at-opponent
  ;; need to make sure this is all working, that it fails successfully etc
  (let [game (init-game p1 p2)]
    (is (= :aircraft-carrier (:result (fire-at-opponent game p1 :player1 :player2))))
    (is (= :aircraft-carrier (:result (fire-at-opponent game p1 :player1 :player2))))
    (is (= :aircraft-carrier (:result (fire-at-opponent game p1 :player1 :player2))))
    (is (= :aircraft-carrier (:result (fire-at-opponent game p1 :player1 :player2))))
    (is (= :aircraft-carrier (:result (fire-at-opponent game p1 :player1 :player2))))
    (is (nil? (:result (fire-at-opponent game p1 :player1 :player2))))
    ;; the last shot is invalid
    (let [{:keys [result updated-game]} (fire-at-opponent game p1 :player1 :player2)]
      (is (nil? result))
      (println updated-game)
      (is (true? (get-in updated-game [:player1 :failed]))))))
