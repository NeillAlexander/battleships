(ns battleships.loader
  (:import [java.io PushbackReader]
           [battleships.engine ShipPosition])
  (:require [clojure.java.io :as io]
            [battleships.game :as game]
            [battleships.engine :as engine]
            [clojail.core :as clojail]
            [clojail.testers :as testers]))

(defn read-ns
  "Returns a vector containing the forms from the file."
  [player-ns-file]
  (with-open [in (PushbackReader. (io/reader player-ns-file))]
    (loop [forms []]
      (if-let [form (read in nil nil)]
        (recur (conj forms form))
        forms))))

(defn ns-form? [form]
  (= (first form) 'ns))

(defn replace-ns [player-ns namespace]
  (vec (map #(if (ns-form? %) `(ns ~(symbol namespace)) %) player-ns)))

(defn add-in-ns [player-ns]
  (conj player-ns '(in-ns 'battleships.client)))

(defn eval-ns
  "Evals the namespace and returns the ns name used."
  ([ns-file]
    (eval-ns ns-file (gensym "player")))
  ([ns-file player-ns]
    (when (find-ns player-ns)
      (remove-ns player-ns))    
    (let [sb (clojail/sandbox testers/secure-tester-without-def :namespace player-ns)]      
      (let [ns-code (if (vector? ns-file) ns-file (read-ns ns-file))] 
        (binding [*ns* *ns*]
          (doall (map sb (filter #(not (ns-form? %)) ns-code)))
          player-ns)))))

(defn update-ns
  "Updates the player namespace, clearing out the original one first."
  [ns-file player-ns]
  (if (find-ns player-ns)
    (println "Ready to remove the namespace")))

(defn valid-place-ship-result? [f]
  (let [result (f (second (first (game/new-ships))))]
    (and (map? result)
         (:square result)
         (:orientation result))))

(defn valid-next-shot-result? [f]
  (let [test-game (game/new-game)
        result (f (engine/new-player-context test-game :player1)
                  (engine/new-player-context test-game :player2))]
    (string? result)))

(defn valid-player-ns?
  "Check to make sure the ns defines the 2 required functions."
  [player-ns]
  (let [place-ship-fn (ns-resolve player-ns 'place-ship)
        next-shot-fn (ns-resolve player-ns 'next-shot)
        valid-place-ship (valid-place-ship-result? place-ship-fn)
        valid-next-shot (valid-next-shot-result? next-shot-fn)]
    (and valid-place-ship valid-next-shot)))

(defn make-player
  "Reify the player using the functions from the namespace."
  ([player-ns]
     (make-player player-ns "Test Player"))
  ([player-ns name]
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

(defn cleanup-ns
  [player-ns]
  (doall (map (partial ns-unmap player-ns) (keys (ns-map player-ns))))
  (remove-ns player-ns)
  nil)
