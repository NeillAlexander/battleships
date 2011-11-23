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
  (next-shot [this player-context opponent-context]
    "Return the coordinate for the next shot. Will be called until a valid coord is returned.")
  (you-won [this player-context opponent-context]
    "Called when this player won the game.")
  (you-lost [this player-context opponent-context]
    "Called when this player lost the game."))

(defn valid-player? [player]
  (satisfies? Player player))

(defn ensure-valid-players [player1 player2]
  (if-not (and (valid-player? player1)
               (valid-player? player2))
    (throw (IllegalArgumentException. "Invalid players"))))

;; Abstract out the attempt to a) practice writing macros and b) make the code more readable
(defmacro attempt
  "Executes attempt-clause repeatedly until it returns a truthy answer. The number of allowed attempts is specified as follows: [:limited-to 100 :when true]"
  [limits attempt-clause else]
  (let [limit-map (apply array-map limits)
        else-clause (rest else)]
    `(loop [num-attempts# 0]
       (if-not (and (> num-attempts# ~(:limited-to limit-map)) ~(:when limit-map))
         (if-let [result# (try 
                            ~attempt-clause
                            (catch Exception e# (println "Caught Exception: " (.getMessage e#))
                              (.printStackTrace e#)))]
           result#
           (recur (inc num-attempts#)))
         ~(first else-clause)))))

(defn place-ship
  "Places the ship on the board, repeating until a valid position is provided or (for a bot) the number of attempts exceeds 100"
  [player-key player-impl game ship-key]
  ;; if it's a bot, give it 100 attempts to make a valid move
  (attempt [:limited-to 100 :when (bot? player-impl)]
    (let [player-data (game player-key)
          ships (:ships player-data)
          ship-pos (ship-position player-impl (ships ship-key))]
      (if-let [updated-player-data (game/place-ship
                                    player-data ship-key (:coord ship-pos) (:orientation ship-pos))]
        (assoc game player-key updated-player-data)))
    (else
     (assoc-in game [player-key :failed] true))))


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

(defn notify-winning-player [game winner-key loser-key player1 player2]
  (let [winner (if (= :player1 winner-key) player1 player2)
        loser (if (= :player2 loser-key) player2 player1)
        winner-context (get-in game [winner-key :context])
        loser-context (get-in game [loser-key :context])]
    (you-won winner winner-context loser-context)
    (you-lost loser loser-context winner-context)))

(defn new-player-context [game player-key]
  (assoc-in game [player-key :context]
            {:shots []
             :last-result nil, 
             :hits [],
             :ships-sunk []}))

(defn calc-result [result sunk]
  (cond
   (and result sunk) sunk
   result :hit
   :else :miss))

(defn conj-data-in-context
  "Update the data at key in context with conj if bool."
  [context key data bool]
  (let [data-store (context key)]
    (if bool
      (conj data-store data)
      data-store)))

(defn update-player-context
  "Update the player context with the result of the last shot."
  [game player-key coord result sunk]
  (let [context (get-in game [player-key :context])
        shots (conj-data-in-context context :shots coord true)
        hits (conj-data-in-context context :hits coord result)        
        ships-sunk (conj-data-in-context  context :ships-sunk sunk sunk)]
    (-> game 
      (assoc-in [player-key :context :last-result] (calc-result result sunk))
      (assoc-in [player-key :context :hits] hits)
      (assoc-in [player-key :context :shots] shots)
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
  (let [num-failures (count-failures player1 player2)
        player1-context (get-in game [:player1 :context])
        player2-context (get-in game [:player2 :context])]
    (cond
     (= 1 num-failures) (let [failed-player (the-failed-player game)]
                          (let [winning-player (if (= :player1 failed-player) :player2 :player1)]
                            (notify-winning-player game winning-player failed-player
                                                   player1-impl player2-impl)))
     (= 2 num-failures) (do
                          (you-lost player1-impl player1-context player2-context)
                          (you-lost player2-impl player2-context player1-context))
     (:winner game) (notify-winning-player game (:winner game) (:loser game) player1-impl player2-impl)
     :else (throw (IllegalStateException. "Unknown finishing condition"))))
  game)


;; fire-at-opponent below here
(defn fire-at-opponent
  "Fire a shell at opponent, returning map of {:result :updated-game}"
  [game player-impl player-key opponent-key]
  ;; if it's a bot playing, then give it 100 attempts to make a valid move
  (attempt [:limited-to 100 :when (bot? player-impl)]
    (let [player-data (game opponent-key)
          board (player-data :board)
          player-context (get-in game [player-key :context])
          opponent-context (get-in game [player-key :context])
          coord (next-shot player-impl player-context opponent-context)]
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
           :coord coord})))
    (else
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
                  (finished (assoc updated-game :winner player-key :loser opponent-key) player1 player2)
                  (recur updated-game turns))
                (recur updated-game turns)))
            ;; missed - continue with next player
            (recur updated-game (drop 2 turns)))
          ;; firing failed - opponent wins
          (finished updated-game player1 player2))))))

(defn generate-stats
  "For now this just returns the game."
  [game]
  (cond
   (get-in game [:player1 :failed]) (assoc game :winner :player2)
   (get-in game [:player2 :failed]) (assoc game :winner :player1)
   :else game))

(defn play
  "Call this to start a new game with the 2 players. Returns the stats for the game."
  [player1 player2]
  (ensure-valid-players player1 player2)
  (let [game (init-game player1 player2)
        finished-game (if (ship-placement-success? game)
                        (run-game-loop game player1 player2)
                        (finished game player1 player2))]
    (generate-stats finished-game)))
