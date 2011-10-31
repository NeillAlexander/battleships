(ns battleships.core
  (:require [battleships.engine :as engine]
            [battleships.board :as board])
  (:import  [battleships.engine ShipPosition])
  (:gen-class :main true))


(defn make-random-cpu-player
  ([name]
     (let [board (board/make-board)
           squares (atom (zipmap (range 0 99)
                                 (map (partial board/transform board) (range 0 99))))
           orientations [:h :v]
           num-shots (atom 0)]
       (reify engine/Player
         (get-name [this] name)
         (bot? [this] true)
         (ship-position [this ship]
           (let [pos (ShipPosition. (get @squares (rand-int (count @squares)))
                                    (orientations (rand-int (count orientations))))]
             (println (str name " trying to place " (:name ship) " at " pos))
             pos))
         (next-shot [this player-context]
           (println (str name " " player-context))
           (let [key (nth (keys @squares) (rand-int (count @squares)))
                 coord (@squares key)]
             (swap! squares dissoc key)
             coord))         
         (you-won [this]
           (println (str name " won in " @num-shots " shots!")))
         (you-lost [this]
           (println (str name " lost!")))))))

(defn -main
  "Sets up the game"
  [& args]
  (engine/play (make-random-cpu-player "cpu1")
               (make-random-cpu-player "cpu2")))

