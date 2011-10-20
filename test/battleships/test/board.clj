(ns battleships.test.board
  (:use [clojure.test]
        [battleships.board]))


(deftest test-transform-coords
  (let [board (make-board 10 10)]
    (is (= 0 (transform board "a1")))
    (is (= 0 (transform board "A1")))
    (is (= 9 (transform board "a10")))
    (is (= 10 (transform board "b1")))
    (is (= 99 (transform board "j10")))))

(deftest test-transform-index
  (let [board (make-board 10 10)]
    (is (= "A1" (transform board 0)))
    (is (= "A10" (transform board 9)))
    (is (= "B1" (transform board 10)))
    (is (= "J10" (transform board 99)))))

(deftest test-squares
  (is (= ["A1" "A2" "A3"] (squares "A1" 3 :h)))
  (is (= ["A1" "A2" "A3" "A4"] (squares "A1" 4 :h)))
  (is (= ["A1" "B1" "C1"] (squares "A1" 3 :v)))
  (is (= ["D3" "E3" "F3" "G3"] (squares "D3" 4 :v))))

(deftest test-valid-square
  (let [board (make-board 10 10)]
    (is (valid-square? board "A1"))
    (is (valid-square? board "A10"))
    (is (valid-square? board "J1"))
    (is (valid-square? board "J10"))
    (is (not (valid-square? board "A0")))
    (is (not (valid-square? board "A11")))
    (is (not (valid-square? board "J0")))
    (is (not (valid-square? board "J11")))))
