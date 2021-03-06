(ns battleships.replay-test
  (:require [battleships.engine :as engine])
  (:use [clojure.test]
        [battleships.replay]))

(def saved-game 
  {:loser :player2, :winner :player1, 
   :player1 {:context {:shots ["I9" "G9" "H3" "F5" "F2" "B10" "I10" "I8" "A2" "D5" "F4" "B3" "B1" "H7" "E9" "B4" "C8" "B7" "A6" "B2" "J8" "E6" "J6" 
                               "C10" "G7" "H8" "E8" "D9" "I5" "D8" "J9" "J4" "A9" "B8" "A5" "I4" "D7" "J2" "A8" "H6" "B5" "A7" "F10" "D4" "C6" "C3" 
                               "E3" "I1" "C4" "H4" "D10" "D6" "F3" "F6" "E10" "G3" "G8" "H5" "J7" "C9" "G1" "C2" "E2" "E5" "I7" "B6" "J3" "H10" "F1" 
                               "D1" "J10" "E4" "A3" "C5" "F7" "F9" "C1" "J1" "A4" "H1" "E1" "H9" "H2" "I6" "C7"], 
                       :last-result :destroyer, 
                       :hits ["G9" "B1" "E9" "C8" "B7" "C10" "D9" "B8" "D10" "E10" "C9" "D1" "F9" "C1" "E1" "H9" "C7"], 
                       :ships-sunk [:patrol-boat :submarine :battleship :aircraft-carrier :destroyer]}, 
             :name "Test Player 1", 
             :board {:width 10, :height 10, 
                     :squares [0 1 1 1 1 0 1 1 1 1 1 1 1 1 1 8 1 1 1 1 1 1 1 1 1 9 1 0 1 1 0 0 17 1 0 9 1 1 1 0 1 1 17 1 1 5 5 4 5 1 1 1 17 1 3 3 3 3
                               3 1 1 0 1 0 1 0 1 1 0 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 0 1 33 33 1 1 1 1 0 1 1 1 1 1 1 0]}, 
             :ships {:aircraft-carrier {:name "aircraft carrier", :key :aircraft-carrier, :length 5, :hits #{"F5" "F6" "F7" "F8" "F9"}, :coord "F5", :orientation :h}, 
                     :battleship {:name "battleship", :key :battleship, :length 4, :hits #{"E6" "E7" "E9"}, :coord "E6", :orientation :h}, 
                     :destroyer {:name "destroyer", :key :destroyer, :length 3, :hits #{"D6" "C6"}, :coord "B6", :orientation :v}, 
                     :submarine {:name "submarine", :key :submarine, :length 3, :hits #{"F3" "E3" "D3"}, :coord "D3", :orientation :v}, 
                     :patrol-boat {:name "patrol boat", :key :patrol-boat, :length 2, :hits #{"I7" "I8"}, :coord "I7", :orientation :h}}}, 
   
   :player2 {:context {:shots ["E7" "E1" "H2" "D9" "F9" "G3" "G5" "J1" "I7" "G8" "H1" "A4" "I3" "C1" "I1" "H5" "H7" "A2" "G10" "H4" "I6" "D4" "C3" "A9" 
                               "F8" "I2" "D3" "A7" "G1" "B5" "F3" "C7" "J2" "B8" "F2" "B2" "H8" "B7" "H6" "I8" "E2" "D6" "H10" "G7" "J4" "B10" "J6" "D8" 
                               "H3" "J9" "A3" "E6" "F1" "E9" "F5" "C5" "A5" "C2" "A10" "I10" "A8" "J5" "C9" "F6" "F7" "C6" "C4" "C10" "B3" "J7" "D7" "B9" 
                               "B4" "F10" "I4" "F4" "E10" "E3" "E4" "B1" "I9" "J8" "E5"], 
                       :last-result :miss, 
                       :hits ["E7" "F9" "I7" "F8" "D3" "F3" "I8" "D6" "E6" "E9" "F5" "F6" "F7" "C6" "E3"], 
                       :ships-sunk [:patrol-boat :aircraft-carrier :submarine]}, 
             :name "CPU", 
             :board {:width 10, :height 10, 
                     :squares [0 1 1 1 1 1 1 1 1 0 5 1 1 1 1 1 33 33 0 1 5 1 1 1 1 1 9 9 9 17 5 0 0 1 1 1 1 1 3 17 5 1 1 1 1 1 0 1 3 17 1 1 1 1 1 1 1 0 
                               3 1 1 0 1 0 0 0 1 1 3 0 1 1 1 1 1 1 1 1 3 1 1 0 0 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1]}, 
             :ships {:aircraft-carrier {:name "aircraft carrier", :key :aircraft-carrier, :length 5, :hits #{"H9" "G9" "F9" "E9" "D9"}, :coord "D9", :orientation :v}, 
                     :battleship {:name "battleship", :key :battleship, :length 4, :hits #{"E1" "D1" "C1" "B1"}, :coord "B1", :orientation :v}, 
                     :destroyer {:name "destroyer", :key :destroyer, :length 3, :hits #{"C7" "C8" "C9"}, :coord "C7", :orientation :h}, 
                     :submarine {:name "submarine", :key :submarine, :length 3, :hits #{"C10" "D10" "E10"}, :coord "C10", :orientation :v}, 
                     :patrol-boat {:name "patrol boat", :key :patrol-boat, :length 2, :hits #{"B7" "B8"}, :coord "B7", :orientation :h}}}})

(deftest test-build-players-from-game
  (let [[p1 p2] (build-players-from-game saved-game)
        replayed-game (engine/play p1 p2)]
    (is (= :player1 (:winner replayed-game)))
    (is (= saved-game replayed-game))))