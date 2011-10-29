(ns battleships.test.game
  (:require [battleships.board :as board])
  (:use [battleships.game]
        [clojure.test]))

;; tests that the board has been updated
(deftest test-place-ship
  (is (= 2 (get-in (place-ship (new-player "Player") :aircraft-carrier "a1" :h)  [:board :squares 0])))
  (is (nil? (place-ship (new-player "Player") :aircraft-carrier "a9" :h))))

(deftest test-all-ships-placed?
  (let [p1 (-> (new-player "p1")
               (place-ship :aircraft-carrier "a1" :h)
               (place-ship :battleship "b1" :h)
               (place-ship :destroyer "c1" :h)
               (place-ship :submarine "d1" :h)
               (place-ship :patrol-boat "e1" :h))]
    (is (all-ships-placed? p1)))
  (let [p2 (-> (new-player "p2")
               (place-ship :aircraft-carrier "a1" :h)
               (place-ship :battleship "b1" :h)
               (place-ship :destroyer "c1" :h)
               (place-ship :patrol-boat "e1" :h))]
    (is (not (all-ships-placed? p2)))))

(deftest test-fire-shell
  (let [p1 (new-player "p1")]
    (is (nil? (fire-shell p1 "a12")))
    (is (board/shelled? (:board (fire-shell p1 "a1")) "a1"))))

(deftest test-update-hits
  (let [p1 (-> (new-player "p1")
               (place-ship :aircraft-carrier "a1" :h)
               (fire-shell "a1")
               (fire-shell "a2")
               (update-hits "a1"))]
    (is (= 1 (get-in p1 [:ships :aircraft-carrier :hits])))
    (is (= 2 (get-in (update-hits p1 "a2") [:ships :aircraft-carrier :hits])))))
