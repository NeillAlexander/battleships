(ns battleships.test.game
  (:use [battleships.game]
        [clojure.test]))

;; tests that the board has been updated
(deftest test-place-ship
  (is (= 2 (get-in (place-ship (new-player "Player") :aircraft-carrier "a1" :h)  [:board :squares 0])))
  (is (nil? (place-ship (new-player "Player") :aircraft-carrier "a9" :h))))
