(ns battleships.board)

;; Data structures
(defn make-board
  "Create a new board of width w and height h."
  [w h]
  {:width w,
   :height h,
   :squares (apply vector (take (* w h) (repeat 0)))})

(defn make-ship
  "key needs to match the tags"
  [name key length]
  {:name name, :key key, :length length})

;; Functions that manipulate the data structures
(defn row
  "return the row for coord, where coord like a3 (would return A)"
  [coord]
  (Character/toUpperCase (first coord)))

(defn column
  "return the column part of the coord, where coord like a3"
  [coord]
  ;; columns start at 1 so need to subtract 0 index vector.
  (Integer/parseInt (apply str (rest coord))))

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
        col-num (dec (column coord))]
    (+ (* row-num width) col-num)))

;; transform 0 to A1 etc
(defmethod transform Integer
  [{:keys [width height] :as board} index]
  (let [row-num (quot index width)
        col-num (rem index width)]
    (str (int-to-row row-num) (inc col-num))))

;; functions that generate the indices of the squares when placing a ship
(defmulti squares
  "Works out the sequence of squares that will be used for the ship."
  (fn [coords length orientation] orientation))

;; horizontal orientation
(defmethod squares :h [coords length _]
  (let [row (row coords)
        col (column coords)]
    (take length (map str (repeat row) (iterate inc col)))))

;; vertical orientation
(defmethod squares :v [coords length _]
  (let [row-num (row-to-int (row coords))
        col (column coords)]
    (take length (map str (map int-to-row (iterate inc row-num)) (repeat col)))))

(defn between [n l h]
  (and (>= n l) (<= n h)))

;; function to validate that square is within boundary (needed for placing ships)
(defn valid-square? [{:keys [width height] :as board} coord]
  (let [row-num (row-to-int (row coord))
        col-num (dec (column coord))]
    (and
     (between row-num 0 (dec height))
     (between col-num 0 (dec width)))))

;; these are the low level tagging functions on which to build things like occupied? etc
;; bit map used for board state
(def tag-bits
  {:shelled 0, :aircraft-carrier 1, :battleship 2, :destroyer 3, :submarine 4, :patrol-boat 5})

(defn sq-index [board coord]
  (transform board coord))

(defn sq-value [board coord]
  ((:squares board) (sq-index board coord)))
 
(defn tag
  "Tag the square at coord with the tag, returning the updated board"
  [tag {:keys [squares] :as board} coord]
  (assoc-in board [:squares] (assoc squares (sq-index board coord)
                                    (bit-set (sq-value board coord) (tag-bits tag)))))

(defn sq-tagged?
  "Check the coord to see if it is tagged."
  [{:keys [squares] :as board} coord tag]
  (bit-test (squares (sq-index board coord)) (tag-bits tag)))

;; functions for placing ship on board
;; need to be able to tell which ship occupies a square
(defn sq-occupied? [board coord]
  (> (sq-value board coord) 1))

(defn sq-empty? [board coord]
  (not (sq-occupied? board coord)))

(defn place-ship
  "Tags the squares with the details of the ship, if not occupied"
  [{:keys [length key] :as ship} board coord orientation]
  (let [sqrs (squares coord length orientation)]
    (if (and (every? (partial sq-empty? board) sqrs)
             (every? (partial valid-square? board) sqrs))
      (reduce (partial tag key) board sqrs))))
