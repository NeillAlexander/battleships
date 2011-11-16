;; The loader is what is used to compile the player. This makes use of Clojail to evaluate
;; the code in a way that will ensure no-one can blow up my computer!
(ns battleships.loader
  (:import [java.io PushbackReader]
           [battleships.engine ShipPosition])
  (:require [clojure.java.io :as io]
            [battleships.game :as game]
            [battleships.engine :as engine]
            [clojail.core :as clojail]
            [clojail.testers :as testers]))

;; Read the code in from the file, and use the reader to build up the Clojure data structure.
(defn read-ns
  "Returns a vector containing the forms from the file."
  [player-ns-file]
  (with-open [in (PushbackReader. (io/reader player-ns-file))]
    (loop [forms []]
      (if-let [form (read in nil nil)]
        (recur (conj forms form))
        forms))))

;; Check whether this form starts with ns. Used to filter out the namespace, since when the code
;; is loaded, we will create a random namespace for it.
(defn ns-form? [form]
  (= (first form) 'ns))

;; Evals the code using Clojail, returning the symbol that represents the namespace.
(defn eval-ns
  "Evals the namespace and returns the ns name used."
  ([ns-file]
    (eval-ns ns-file (gensym "player")))
  ([ns-file player-ns]
    (when (find-ns player-ns)
      (remove-ns player-ns))    
    (let [sb (clojail/sandbox testers/secure-tester-without-def :namespace player-ns :max-defs 1000)]      
      (let [ns-code (if (vector? ns-file) ns-file (read-ns ns-file))] 
        (binding [*ns* *ns*]
          (doall (map sb (filter #(not (ns-form? %)) ns-code)))
          player-ns)))))

;; Once a player has been created, it's possible to update it. To do so, the previous
;; namespace must be provided.
(defn update-ns
  "Updates the player namespace, clearing out the original one first."
  [ns-file player-ns]
  (if (find-ns player-ns)
    (println "Ready to remove the namespace")))

;; Ensure that the the place-ship function succeeded. This is a simple validation
;; that the player is returning sensible data.
(defn valid-place-ship-result? [f]
  (let [result (f (second (first (game/new-ships))))]
    (and (map? result)
         (:square result)
         (:orientation result))))

;; Ensure that the next-shot function is returning something that looks sensible.
(defn valid-next-shot-result? [f]
  (let [test-game (game/new-game)
        result (f (engine/new-player-context test-game :player1)
                  (engine/new-player-context test-game :player2))]
    (string? result)))

;; Make sure that the code defines the functions that are required.
(defn valid-player-ns?
  "Check to make sure the ns defines the 2 required functions."
  [player-ns]
  (let [place-ship-fn (ns-resolve player-ns 'place-ship)
        next-shot-fn (ns-resolve player-ns 'next-shot)
        valid-place-ship (valid-place-ship-result? place-ship-fn)
        valid-next-shot (valid-next-shot-result? next-shot-fn)]
    (and valid-place-ship valid-next-shot)))

;; Reifies the Player protocol, using the functions that are provided by the submitted namespace.
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

;; Get rid of a player namespace.
(defn cleanup-ns
  [player-ns]
  (doall (map (partial ns-unmap player-ns) (keys (ns-map player-ns))))
  (remove-ns player-ns)
  nil)
