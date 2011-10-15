(ns battleships.test.board
  (:use [clojure.test])
  (:require [battleships.board] :reload-all))


(deftest transform-coords
  (let [board (make-board 10 10)]
    (is (= 0 (transform board "a1")))
    (is (= 0 (transform board "A1")))
    (is (= 9 (transform board "a10")))
    (is (= 10 (transform board "b1")))
    (is (= 99 (transform board "j10")))
    (is (= nil (transform board "a12")))
    (is (= nil (transform board "k1")))))

(deftest transform-index
  (let [board (make-board 10 10)]
    (is (= "A1" (transform board 0)))
    (is (= "A10" (transform board 9)))
    (is (= "B1" (transform board 10)))
    (is (= "J10" (transform board 99)))))
