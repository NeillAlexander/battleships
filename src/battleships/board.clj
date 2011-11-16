;; This provides the low level DSL for the game - all the functions that are required to set up the board,
;; place ships, fire at ships etc. There is no game logic in here at all.
(ns battleships.board)

;; Data structures
(defn make-board
  "Create a new board of width w and height h."
  ([]
     (make-board 10 10))
  ([w h]
     {:width w,
      :height h,
      :squares (apply vector (take (* w h) (repeat 0)))}))

(defn make-ship
  "key needs to match the tags"
  [name key length]
  {:name name, :key key, :length length, :hits #{}, :coord nil, :orientation nil})

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
(defmethod transform Long
  [{:keys [width height] :as board} index]
  (let [row-num (quot index width)
        col-num (rem index width)]
    (str (int-to-row row-num) (inc col-num))))

;; functions that generate the indices of the squares when placing a ship
(defmulti squares
  "Works out the sequence of squares that will be used for the ship."
  (fn [coords length orientation]
    (if (#{:v :h} orientation)
      orientation)))

;; horizontal orientation
(defmethod squares :h [coords length _]
  (if coords
    (let [row (row coords)
          col (column coords)]
      (take length (map str (repeat row) (iterate inc col))))))

;; vertical orientation
(defmethod squares :v [coords length _]
  (let [row-num (row-to-int (row coords))
        col (column coords)]
    (take length (map str (map int-to-row (iterate inc row-num)) (repeat col)))))

(defmethod squares :invalid [_ _ _]
  nil)

(defmethod squares nil [_ _ _]
  nil)

(defn between [n l h]
  (and (>= n l) (<= n h)))

;; function to validate that square is within boundary (needed for placing ships)
(defn valid-square? [{:keys [width height] :as board} coord]
  (if coord
    (let [row-num (row-to-int (row coord))
          col-num (dec (column coord))]
      (and
       (between row-num 0 (dec height))
       (between col-num 0 (dec width))))))

(defn invalid-square? [board coord]
  (not (valid-square? board coord)))

;; these are the low level tagging functions on which to build things like occupied? etc
;; bit map used for board state

;; the ships in the game
(def ship-keys [:aircraft-carrier :battleship :destroyer :submarine :patrol-boat])

;; all the tags
(def tag-keys (into [:shelled] ship-keys))

;; defines the bit for each tag
(def tag-bits (into {} (map-indexed (fn [bit-idx key] [key bit-idx]) tag-keys)))

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

(defn valid-ship-coords? [coord orientation]
  (and (and coord orientation)
       (#{:h :v} orientation)))

(defn place-ship
  "Tags the squares with the details of the ship, if not occupied"
  [{:keys [length key] :as ship} board coord orientation]
  (when (valid-ship-coords? coord orientation)
    (let [sqrs (squares coord length orientation)]
      (if (and (every? (partial valid-square? board) sqrs)
               (every? (partial sq-empty? board) sqrs))
        (reduce (partial tag key) board sqrs)))))

(defn fire-shell
  "This is the command for firing a shell at a square, specified by coord."
  [board coord]
  (tag :shelled board coord))

(defn ship-key-at
  "Returns the key for the ship at coord, or nil if none there."
  [board coord]
  (first (filter (partial sq-tagged? board coord) ship-keys)))

(defn shelled?
  [board coord]
  (sq-tagged? board coord :shelled))

(defn not-shelled? [board coord]
  (not (shelled? board coord)))

(defn hit?
  "Was there a ship hit at coord? If yes, return the key of the ship that was hit."
  [board coord]
  (if (and (sq-occupied? board coord) (shelled? board coord))
    (ship-key-at board coord)))


