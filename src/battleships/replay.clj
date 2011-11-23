;; Provides a function to generate 2 players from a previous game i.e. the game data structure
;; that is returned by the engine.
(ns battleships.replay
  (:import [battleships.engine ShipPosition])
  (:require [battleships.engine :as engine]))

(defn- make-ship-chooser 
  [ships]
  (fn [{:keys [key] :as ship}]    
    (let [ship-data (ships key)]
      {:square (:coord ship-data) :orientation (:orientation ship-data)})))

(defn- make-shot-chooser
  [shots]
  (let [index (atom -1)]
    (fn []
      (shots (swap! index inc)))))

(defn- build
  "Builds the player from the game."
  [player-key game]
  (let [ship-chooser-fn (make-ship-chooser (get-in game [player-key :ships]))
        shot-chooser-fn (make-shot-chooser (get-in game [player-key :context :shots]))
        name (get-in game [player-key :name])] 
    (reify engine/Player
      (get-name [this] name)
      (bot? [this] true)
      (ship-position [this ship]
                     (let [pos (ship-chooser-fn ship)]
                       (ShipPosition. (:square pos) (:orientation pos))))
      (next-shot [this player-context opponent-context]
                (shot-chooser-fn))         
      (you-won [this {:keys [last-result hits ships-sunk shots]} opponent-context]
                (println name " won in " (count shots) " shots!"))
      (you-lost [this {:keys [last-result hits ships-sunk shots]} opponent-context]
                (println name " lost in " (count shots) " shots!")))))

(defn build-players-from-game 
  "Builds the 2 players based on the shots etc from the game."
  [from-game]
  [(build :player1 from-game) (build :player2 from-game)])