(ns battleships.engine
  "This is the inteface that enables different implementations to be plugged in."
  (:require [battleships.board :as board]
            [battleships.game :as game]))

(defrecord ShipPosition
    [coord orientation]
  Object
  (toString [this]
    (str "[" coord "," orientation "]")))

(defprotocol Player
  "For a game of battleships we need to plug 2 Players into the engine."
  (get-name [this]
    "Should return the name of the player.")
  (bot? [this]
    "Must return true if this is a bot.")
  (ship-position [this ship]
    "Return the ShipPosition for the ship. This will be called for each ship until all ships are placed on board.")
  (next-shot [this player-context]
    "Return the coordinate for the next shot. Will be called until a valid coord is returned.")
  (you-won [this]
    "Called when this player won the game.")
  (you-lost [this]
    "Called when this player lost the game."))

(defn valid-player? [player]
  (satisfies? Player player))

(defn ensure-valid-players [player1 player2]
  (if-not (and (valid-player? player1)
               (valid-player? player2))
    (throw (IllegalArgumentException. "Invalid players"))))

(defn place-ship
  "Places the ship on the board, repeating until a valid position is provided or (for a bot) the number of attempts exceeds 100"
  [player-key player-impl game ship-key]
  (loop [num-attempts 0
         player-data (game player-key)
         ships (:ships player-data)]
    ;; if the number of attempts exceeds 100 for a bot, mark it as failed
    (if (and (> num-attempts 100) (bot? player-impl))
      (assoc-in game [player-key :failed] true)
      (let [ship-pos (ship-position player-impl (ships ship-key))
            updated-player-data (game/place-ship
                                 player-data ship-key (:coord ship-pos) (:orientation ship-pos))]
        (if updated-player-data
          (assoc game player-key updated-player-data)
          (recur (inc num-attempts) player-data ships))))))

(defn place-ships
  "Calls back to ship-position for every ship until all have been placed successfully."
  [game player-key player-impl]
  (reduce (partial place-ship player-key player-impl) game board/ship-keys))

(defn first-failed-second-ok? [player1 player2]
  (and (:failed player1) (not (:failed player2))))

(defn the-failed-player [{:keys [player1 player2]}]
  (cond
   (first-failed-second-ok? player1 player2) :player1
   (first-failed-second-ok? player2 player1) :player2
   :else nil))

(defn count-failures [& players]
  (reduce (fn [n player] (if (:failed player) (inc n) n)) 0 players))

(defn notify-winning-player [winner-key loser-key player1 player2]
  (let [winner (if (= :player1 winner-key) player1 player2)
        loser (if (= :player2 loser-key) player2 player1)]
    (you-won winner)
    (you-lost loser)))

(defn new-player-context [game player-key]
  (assoc-in game [player-key :context]
            {:last-shot nil, :last-result nil, :hits [],
             :misses [], :ships-sunk []}))

(defn calc-result [result sunk]
  (cond
   (and result sunk) sunk
   result :hit
   :else :miss))

(defn update-hits [hits coord result sunk]
  (if result
    (conj hits coord)
    hits))

(defn update-misses [misses coord result]
  (if-not result
    (conj misses coord)))

(defn update-ships-sunk [ships-sunk sunk]
  (if sunk
    (conj ships-sunk sunk)
    ships-sunk))

(defn update-player-context
  "Update the player context with the result of the last shot."
  [game player-key coord result sunk]
  (let [context (get-in game [player-key :context])
        hits (update-hits (:hits context) coord result sunk)
        misses (update-misses (:misses context) coord result)
        ships-sunk (update-ships-sunk (:ships-sunk context) sunk)]
    (-> (assoc-in game [player-key :context :last-shot] coord)
        (assoc-in [player-key :context :last-result] (calc-result result sunk))
        (assoc-in [player-key :context :hits] hits)
        (assoc-in [player-key :context :misses] misses)
        (assoc-in [player-key :context :ships-sunk] ships-sunk))))

(defn init-game [player1 player2]
  (-> (game/new-game (get-name player1) (get-name player2))
      (new-player-context :player1)
      (new-player-context :player2)
      (place-ships :player1 player1)
      (place-ships :player2 player2)))


(defn ship-placement-success? [{:keys [player1 player2]}]
  (= 0 (count-failures player1 player2)))

(defn finished
  "Checks the end game conditions and notifies accordingly."
  [{:keys [player1 player2] :as game} player1-impl player2-impl]
  (let [num-failures (count-failures player1 player2)]
    (cond
     (= 1 num-failures) (let [failed-player (the-failed-player game)]
                          (let [winning-player (if (= :player1 failed-player) :player2 :player1)]
                            (notify-winning-player winning-player failed-player
                                                   player1-impl player2-impl)))
     (= 2 num-failures) (do
                          (you-lost player1-impl)
                          (you-lost player2-impl))
     :else (throw (IllegalStateException. "Unknown finishing condition")))))

(defn fire-at-opponent
  "Fire a shell at opponent, returning map of {:result :updated-game}"
  [game player-impl player-key opponent-key]
  (loop [num-attempts 0
         player-data (game opponent-key)
         board (player-data :board)]
    (if-not (and (> num-attempts 100) (bot? player-impl))
      (let [player-context (get-in game [player-key :context])
            coord (next-shot player-impl player-context)]
        (if-let [updated-player-data (game/fire-shell player-data coord)]
          (let [result (board/hit? (updated-player-data :board) coord)
                updated-player-data (if result
                                      (game/update-hits updated-player-data coord)
                                      updated-player-data)
                sunk (game/sunk? updated-player-data result)] 
            {:updated-game (update-player-context (assoc game opponent-key updated-player-data)
                                                  player-key
                                                  coord
                                                  result
                                                  sunk),
             :result result
             :sunk sunk
             :coord coord})
          (recur (inc num-attempts) player-data board)))
      {:updated-game (assoc-in game [player-key :failed] true)
       :result nil
       :sunk nil
       :coord nil})))

(defn run-game-loop
  "The main loop that asks for moves until someone wins or a bot exceeds the number of allowed errors"
  [game player1 player2]
  (loop [game game
         turns (cycle [:player1 player1 :player2 player2])]
    (let [player-key (first turns)
          player-impl (second turns)
          opponent-key (nth turns 2)
          opponent-impl (nth turns 3)]
      ;; fire shell at opponent
      (if-let [{:keys [updated-game result coord sunk]}
               (fire-at-opponent game player-impl player-key opponent-key)]
        (if-not (get-in updated-game [player-key :failed])
          (if result
            (let [opponent-data (updated-game opponent-key)]
              (if sunk
                ;; if it sunk something, have I won?
                (if (game/all-ships-sunk? opponent-data)
                  (notify-winning-player player-key opponent-key player1 player2)
                  (recur updated-game (drop 2 turns)))
                (recur updated-game (drop 2 turns))))
            ;; missed - continue with next player
            (recur updated-game (drop 2 turns)))
          ;; firing failed - opponent wins
          (finished game player1 player2))))))

(defn play
  "Call this to start a new game with the 2 players."
  [player1 player2]
  (ensure-valid-players player1 player2)
  (let [game (init-game player1 player2)]
    ;; check to see if either player failed. If one failed and the other didn't then we have a winner
    (if (ship-placement-success? game)
      (run-game-loop game player1 player2)
      (finished game player1 player2))))
