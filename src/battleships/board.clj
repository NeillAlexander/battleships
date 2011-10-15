(ns battleships.board)

(defn make-board
  "Create a new board of width w and height h."
  [w h]
  {:width w,
   :height h,
   :squares (apply vector (take (* w h) (repeat 0)))})

(defn row
  "return the row for coord, where coord like a3 (would return A)"
  [coord]
  (Character/toUpperCase (first coord)))

(defn column
  "return the column part of the coord, where coord like a3"
  [coord]
  ;; columns start at 1 so need to subtract 0 index vector.
  (dec (Integer/parseInt (apply str (rest coord)))))

(defn row-to-int
  "Convert row character (upper case) to an int for indexing."
  [c]
  ;; Just subtract 65 from the character as int
  (- (int c) 65))

(defn int-to-row
  "Opposite conversion from row-to-int"
  [n]
  (char (+ 65 n)))

;; Create the 2 transform methods which will transform co-ordinates to vector index and vice versa
(defmulti transform (fn [board pos & others] (class pos)))

;; transform A1 to 0 etc
(defmethod transform String
  [{:keys [width height] :as board} coord]
  (let [row-num (row-to-int (row coord))
        col-num (column coord)]
    (if-not (or (>= row-num width) (>= col-num height))
      (+ (* row-num width) col-num))))

;; transform 0 to A1 etc
(defmethod transform Integer
  [{:keys [width height] :as board} index]
  (let [row-num (quot index width)
        col-num (rem index width)]
    (str (int-to-row row-num) (inc col-num))))
