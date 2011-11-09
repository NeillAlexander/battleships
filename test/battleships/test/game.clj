(ns battleships.test.game
  (:require [battleships.board :as board])
  (:use [battleships.game]
        [clojure.test]))

;; tests that the board has been updated
(deftest test-place-ship
  (is (= 2 (get-in (place-ship (new-player "Player") :aircraft-carrier "a1" :h)  [:board :squares 0])))
  (is (nil? (place-ship (new-player "Player") :aircraft-carrier "a9" :h)))
  (is (nil? (place-ship (new-player "Player") :aircraft-carrier nil :h)))
  (is (nil? (place-ship (new-player "Player") :aircraft-carrier "a1" nil)))
  (is (nil? (place-ship (new-player "Player") :aircraft-carrier "a1" :x)))
  (is (nil? (place-ship (new-player "Player") :aircraft-carrier nil nil))))

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
    (is (nil? (fire-shell p1 nil)))
    (is (board/shelled? (:board (fire-shell p1 "a1")) "a1"))))

(deftest test-update-hits
  (let [p1 (-> (new-player "p1")
               (place-ship :aircraft-carrier "a1" :h)
               (fire-shell "a1")
               (fire-shell "a2")
               (update-hits "a1"))]
    (is (= 1 (count (get-in p1 [:ships :aircraft-carrier :hits]))))
    (is (= 2 (count (get-in (update-hits p1 "a2") [:ships :aircraft-carrier :hits]))))))

(defn fire-and-update [player coord]
  (update-hits (fire-shell player coord) coord))

(deftest test-sunk?
  (let [p1 (-> (new-player "p1")
               (place-ship :aircraft-carrier "a1" :h)
               (fire-shell "a1")
               (fire-shell "a2")
               (update-hits "a1")
               (update-hits "a2")
               (place-ship :patrol-boat "b1" :h)
               (fire-shell "b1")
               (fire-shell "b2")
               (update-hits "b1")
               (update-hits "b2"))]
    (is (sunk? p1 :patrol-boat))
    (is (not (sunk? p1 :aircraft-carrier)))
    (is (= 1 (count-sunk-ships p1)))))

(deftest test-all-ships-sunk?
  (let [p1 (-> (new-player "p1")
               (place-ship :aircraft-carrier "a1" :h)
               (place-ship :battleship "b1" :h)
               (place-ship :destroyer "c1" :h)
               (place-ship :submarine "d1" :h)
               (place-ship :patrol-boat "e1" :h)
               (fire-and-update "a1")
               (fire-and-update "a2")
               (fire-and-update "a3")
               (fire-and-update "a4")
               (fire-and-update "a5")
               (fire-and-update "b1")
               (fire-and-update "b2")
               (fire-and-update "b3")
               (fire-and-update "b4")
               (fire-and-update "c1")
               (fire-and-update "c2")
               (fire-and-update "c3")
               (fire-and-update "d1")
               (fire-and-update "d2")
               (fire-and-update "d3")
               (fire-and-update "e1")
               (fire-and-update "e2"))]
    (is (all-ships-placed? p1))
    (is (all-ships-sunk? p1))))

;; one missing hence not all sunk
(deftest test-all-ships-not-sunk?
  (let [p1 (-> (new-player "p1")
               (place-ship :aircraft-carrier "a1" :h)
               (place-ship :battleship "b1" :h)
               (place-ship :destroyer "c1" :h)
               (place-ship :submarine "d1" :h)
               (place-ship :patrol-boat "e1" :h)
               (fire-and-update "a1")
               (fire-and-update "a2")
               (fire-and-update "a3")
               (fire-and-update "a4")
               (fire-and-update "a5")
               (fire-and-update "b1")
               (fire-and-update "b2")
               (fire-and-update "b3")
               (fire-and-update "b4")
               (fire-and-update "c1")
               (fire-and-update "c2")
               (fire-and-update "c3")
               (fire-and-update "d1")
               (fire-and-update "d2")
               (fire-and-update "e1")
               (fire-and-update "e2"))]
    (is (all-ships-placed? p1))
    (is (not (all-ships-sunk? p1)))))
