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
    (is (not (valid-square? board "J11")))
    (is (not (valid-square? board "K6")))))

(deftest test-tagging
  (let [board (make-board 10 10)]
    (is (sq-tagged? (tag :shelled board "a1") "a1" :shelled))
    (is (not (sq-tagged? (tag :shelled board "a1") "a2" :shelled)))))

(deftest test-occupied
  (let [board (make-board 10 10)]
    (is (sq-empty? board "a1"))
    (is (sq-occupied? (tag :aircraft-carrier board "a1") "a1"))
    (is (not (sq-occupied? (tag :shelled board "a1") "a1")))))

(deftest test-place-ship-horizontally
  (let [ship (make-ship "test" :battleship 3)
        board (place-ship ship (make-board 10 10) "a1" :h)]
    (is (sq-occupied? board "a1"))
    (is (sq-occupied? board "a2"))
    (is (sq-occupied? board "a3"))
    (is (sq-empty? board "a4"))
    (is (sq-empty? board "b1"))
    (is (sq-empty? board "b2"))
    (is (sq-empty? board "b3"))))

(deftest test-place-ship-vertically
  (let [ship (make-ship "test" :battleship 3)
        board (place-ship ship (make-board 10 10) "a1" :v)]
    (is (sq-occupied? board "a1"))
    (is (sq-occupied? board "b1"))
    (is (sq-occupied? board "c1"))
    (is (sq-empty? board "d1"))
    (is (sq-empty? board "a2"))
    (is (sq-empty? board "b2"))
    (is (sq-empty? board "c2"))
    ;; make sure can't place a ship on top of it
    (is (nil? (place-ship ship board "a1" :h)))
    (is (nil? (place-ship ship board "b1" :h)))
    (is (nil? (place-ship ship board "c1" :h)))
    (is (nil? (place-ship ship board "a1" :v)))))

(deftest test-fire-shell
  (let [ship (make-ship "test" :battleship 3)
        board (place-ship ship (make-board 10 10) "a1" :v)]
    (is (not (hit? board "a1")))
    (is (hit? (fire-shell board "a1") "a1"))
    (is (not (hit? (fire-shell board "a2") "a2")))))

(deftest test-ship-key-at
  (let [ship (make-ship "test" :battleship 3)
        board (place-ship ship (make-board 10 10) "a1" :v)]
    (is (= (ship-key-at board "a1") :battleship))
    (is (nil? (ship-key-at board "a2")))))

(deftest test-place-ship-vertically-out-of-bounds
  (let [ship (make-ship "test" :aircraft-carrier 5)
        board (place-ship ship (make-board 10 10) "g6" :v)]
    (is (nil? board))))
