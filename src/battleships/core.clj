;; Contains the main method. When this is run from the comand line a game is set up with 2 random
;; players. It's more for testing than anything else. In the dojo the intention would be to run
;; a server on one machine, and just provide everyone with the source code so that they can
;; submit players using client.clj.
(ns battleships.core
  (:require [battleships.engine :as engine]
            [battleships.board :as board]
            [battleships.demo :as demo])
  (:import  [battleships.engine ShipPosition])
  (:gen-class :main true))


(defn make-random-cpu-player
  ([name]
     (reify engine/Player
       (get-name [this] name)
       (bot? [this] true)
       (ship-position [this ship]
         (let [pos (demo/place-ship ship)]
           (ShipPosition. (:square pos) (:orientation pos))))
       (next-shot [this {:keys [last-result hits ships-sunk] :as context}
                   opponent-context]
         (demo/next-shot context opponent-context))         
       (you-won [this {:keys [last-result hits ships-sunk]} opponent-context])
       (you-lost [this player-context opponent-context]))))


(defn test-player [player]
  (engine/play player (make-random-cpu-player "cpu")))

(defn -main
  "Sets up the game"
  [& args]
  (engine/play (make-random-cpu-player "cpu1")
               (make-random-cpu-player "cpu2")))

