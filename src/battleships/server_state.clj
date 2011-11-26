(ns battleships.server-state
  (:use [clojure.pprint])
  (:require [battleships.engine :as engine]
            [battleships.core :as core]))

;; Server state
(def ^{:private true} players (ref {}))
(def ^{:private true} players-by-name (ref {}))
(def ^{:private true} match-tracker (atom {}))

(defn num-players [] (count @players))

(defn num-matchups [] (count @match-tracker))

(defn get-players [] @players)

(defn get-player [name]
  (@players-by-name name))

(defn get-match-tracker [] @match-tracker)

(defn player-exists? [name]
  (@players-by-name name))

;; The data structure for all the players that are submitted, used to track wins etc.
(defn make-registered-player [player player-ns code]
  (with-open [sw (java.io.StringWriter.)]
    (pprint code sw)
    {:name (engine/get-name player)
     :namespace player-ns
     :impl player
     :played 0
     :won 0
     :lost 0
     :code (.toString sw)
     :created (java.util.Date.)}))

(defn- init-match! 
  [match-id]
  (swap! match-tracker assoc match-id (atom 0)))

(defn- same-players? [player1 player2]
  (= (:namespace player1) (:namespace player2)))

;; When a new player is added we create a match up between it and all other players. This
;; is used to track and schedule all the required matches.
(defn- create-match-up!
  ([player1 player2]
     (create-match-up! player1 player2 true))
  ([player1 player2 match1?]
     (let [match-id (str (:namespace player1) "-" (:namespace player2))]
       (init-match! match-id)
       (if match1? (recur player2 player1 false)))))

;; Set of functions to update the server state after matches have been played.
(defn update-match-tracker! [new-player]
  (doseq [player (vals @players)]
    (if-not (same-players? player new-player)
      (create-match-up! player new-player))))

(defn update-winner! [winner-ns]
  (dosync
   (alter players update-in [winner-ns :won] inc)
   (alter players update-in [winner-ns :played] inc)))

(defn update-loser! [loser-ns]
  (dosync
   (alter players update-in [loser-ns :lost] inc)
   (alter players update-in [loser-ns :played] inc)))

(defn record-match-result!
  [player1-ns player2-ns winner]
  (cond
   (= :player1 winner) (do (update-winner! player1-ns)
                           (update-loser! player2-ns))
   (= :player2 winner) (do (update-winner! player2-ns)
                           (update-loser! player1-ns))))

(defn- inc-if-below [limit current-value]
  (if (<= current-value limit)
    (inc current-value)
    current-value))

;; Used to keep going if all the matches haven't been played.
(defn inc-match-count-if-below-limit!
  "Returns truthy if the number of matches was incremented, else falsey."
  [match-id limit]
  (let [new-total (swap! (@match-tracker match-id) (partial inc-if-below limit))]
    (if (<= new-total limit)
      new-total
      nil)))

(defn existing-player-id-matches? [name id]
  (if-let [existing-player (player-exists? name)]
    (= (symbol id) (:namespace existing-player))))

;; Called when a player is submitted.
(defn register-player!
  [player player-ns code]
  (let [player-name (engine/get-name player)
        already-registered (player-exists? player-name)
        registered-player (if already-registered 
                            (assoc (@players (str player-ns)) :updated (java.util.Date.))
                            (make-registered-player player player-ns code))]
    (dosync
      (alter players assoc (str player-ns) registered-player)
      (alter players-by-name assoc (:name registered-player) registered-player))
    (when-not already-registered
      (update-match-tracker! registered-player))))

(defn create-default-cpu-player []
  ;; always have the computer in here to play against people
  (register-player! (core/make-random-cpu-player "cpu1") 'battleships.core "HIDDEN"))

(defn reset-state! 
  []
  (dosync
    (ref-set players {})
    (ref-set players-by-name {})
    (reset! match-tracker {}))
  (create-default-cpu-player))