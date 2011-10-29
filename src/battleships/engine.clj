(ns battleships.engine
  "This is the inteface that enables different implementations to be plugged in."
  (:require [battleships.board :as board]
            [battleships.game :as game]))

(defrecord ShipPosition [coord orientation])

(defprotocol Player
  "For a game of battleships we need to plug 2 Players into the engine."
  (get-name [this]
    "Should return the name of the player.")
  (ship-position [this ship-key]
    "Return the ShipPosition for the ship. This will be called for each ship until all ships are placed on board.")
  (next-shot [this]
    "Return the coordinate for the next shot. Will be called until a valid coord is returned.")
  (shot-result [this coord result]
    "Called after the result of the shot is known. Result will be either :hit, :miss, or one of the ship keys if the ship was sunk e.g. :aircraft-carrier.")
  (won [this]
    "Called when this player won the game.")
  (lost [this]
    "Called when this player lost the game."))

(defn- valid-player? [player]
  (satisfies? Player player))

(defn- ensure-valid-players [player1 player2]
  (if-not (and (valid-player? player1)
               (valid-player? player2))
    (throw (IllegalArgumentException. "Invalid players"))))

(defn play
  "Call this to start a new game with the 2 players."
  [player1 player2]
  (ensure-valid-players player1 player2)
  (let [game (game/new-game (get-name player1) (get-name player2))]
    game))
