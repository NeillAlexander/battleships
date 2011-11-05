(ns battleships.client
  (:import [java.io PushbackReader]
           [battleships.engine ShipPosition])
  (:require [clojure.java.io :as io]
            [battleships.core :as core]
            [battleships.game :as game]
            [battleships.engine :as engine]))

(defn- read-ns
  "Returns a vector containing the forms from the file."
  [player-ns-file]
  (with-open [in (PushbackReader. (io/reader player-ns-file))]
    (loop [forms []]
      (if-let [form (read in nil nil)]
        (recur (conj forms form))
        forms))))

(defn- ns-form? [form]
  (= (first form) 'ns))

(defn- replace-ns [player-ns namespace]
  (vec (map #(if (ns-form? %) `(ns ~(symbol namespace)) %) player-ns)))

(defn- add-in-ns [player-ns]
  (conj player-ns '(in-ns 'battleships.client)))

(defn- eval-ns
  "Evals the namespace and returns the ns name used."
  [ns-file]
  (let [ns (gensym "player")]
    (doall (map eval (add-in-ns (replace-ns  (read-ns ns-file) ns))))
    ns))

(defn- valid-place-ship-result? [f]
  (let [result (f (second (first (game/new-ships))))]
    (and (map? result)
         (:square result)
         (:orientation result))))

(defn- valid-next-shot-result? [f]
  (let [test-game (game/new-game)
        result (f (engine/new-player-context test-game :player1)
                  (engine/new-player-context test-game :player2))]
    (string? result)))

(defn- valid-player-ns?
  "Check to make sure the ns defines the 2 required functions."
  [player-ns]
  (let [place-ship-fn (ns-resolve player-ns 'place-ship)
        next-shot-fn (ns-resolve player-ns 'next-shot)
        valid-place-ship (valid-place-ship-result? place-ship-fn)
        valid-next-shot (valid-next-shot-result? next-shot-fn)]
    (and valid-place-ship valid-next-shot)))

(defn- make-player
  "Reify the player using the functions from the namespace."
  [player-ns]
  (let [name "Test player"]
    (reify engine/Player
      (get-name [this] name)
      (bot? [this] true)
      (ship-position [this ship]
        (let [pos ((ns-resolve player-ns 'place-ship) ship)]
          (ShipPosition. (:square pos) (:orientation pos))))
      (next-shot [this player-context opponent-context]
        ((ns-resolve player-ns 'next-shot) player-context opponent-context))         
      (you-won [this {:keys [last-shot last-result hits misses ships-sunk]} opponent-context]
        (println (str name " " last-shot " = " last-result ", ships sunk = " ships-sunk))
        (println (str name " won in " (+ (count hits) (count misses)) " shots!")))
      (you-lost [this player-context opponent-context]
        (println (str name " lost!"))))))

(defn- cleanup-ns
  [player-ns]
  (doall (map (partial ns-unmap player-ns) (keys (ns-map player-ns))))
  nil)

(defn test-player
  "player-ns is the path to the file that contains the namespace definition. This is for use when developing the player locally and want to make sure that it can play a game."
  [player-ns-file]
  (if-let [player-ns (eval-ns player-ns-file)]
    (do
      (if (valid-player-ns? player-ns)
        (core/test-player (make-player player-ns))
        (println "invalid player"))
      (cleanup-ns player-ns))))

(defn submit-player
  "Submit your player to the server."
  [player-ns server-address])
