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

(defn make-test-player
  ([name positions-map result-atom]
     ;; default shots first 5 sink ac, next misses, last fails
     (make-test-player name positions-map result-atom ["a1" "a2" "a3" "a4" "a5" "a6" "a11"]))
  ([name positions-map result-atom shots]
     (let [num-shots (atom 0)]
       (reify Player
         (get-name [this] name)
         (bot? [this] true)
         (ship-position [this ship] (positions-map (:key ship)))
         (next-shot [this]
           (let [shot (nth shots @num-shots)]
             (if (< @num-shots (dec (count shots)))
               (swap! num-shots inc))          
             shot))
         (shot-result [this coord result]
           (println (str "Shot result for " name ": " coord " " result)))
         (you-won [this]
           (println (str name " won!"))
           (reset! result-atom true))
         (you-lost [this]
           (println (str name " lost!"))
           (reset! result-atom false))))))

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
      (is (true? (get-in updated-game [:player1 :failed]))))))


;; runs a full game loop where both players have the same set up and fire at the same squares
;; hence player1 will win because it goes first
;; there are also a couple of misses and one invalid square thrown in
(deftest test-run-game-loop
  (let [shots ["a1" "a2" "a3" "a4" "a5"
               "b1" "b2" "b3" "b4" "b5" "b6" "b20"
               "c1" "c2" "c3"
               "d1" "d2" "d3"
               "e1" "e2"]
        player1-won (atom nil)
        player1 (make-test-player "player1" ship-positions player1-won shots)
        player2-won (atom nil)
        player2 (make-test-player "player2" ship-positions player2-won shots)]
    (run-game-loop (init-game player1 player2) player1 player2)
    (is (true? @player1-won))
    (is (false? @player2-won))))
