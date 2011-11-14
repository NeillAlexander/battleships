(ns battleships.server
  (:use compojure.core)
  (:require [battleships.loader :as loader]
            [battleships.view :as view]
            [battleships.core :as core]
            [battleships.engine :as engine]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [ring.util.response :as resp]
            [clojure.string :as string]))


(def players (ref {}))
(def players-by-name (ref {}))
(def match-tracker (atom {}))

(defn make-registered-player [player player-ns]
  {:name (engine/get-name player)
   :namespace player-ns
   :impl player
   :played 0
   :won 0
   :lost 0})

(defn same-players? [player1 player2]
  (= (:namespace player1) (:namespace player2)))

(defn create-match-up!
  ([player1 player2]
     (create-match-up! player1 player2 true))
  ([player1 player2 match1?]
     (let [match-id (str (:namespace player1) "-" (:namespace player2))]
       (swap! match-tracker assoc match-id 0)
       (if match1? (recur player2 player1 false)))))

(defn update-match-tracker! [new-player]
  (doseq [player (vals @players)]
    (if-not (same-players? player new-player)
      (create-match-up! player new-player))))

(defn update-winner! [winner-ns]
  (println (str "Winner is " winner-ns))
  (dosync
   (alter players update-in [winner-ns :won] inc)
   (alter players update-in [winner-ns :played] inc)))

(defn update-loser! [loser-ns]
  (println (str "Loser is " loser-ns))
  (dosync
   (alter players update-in [loser-ns :lost] inc)
   (alter players update-in [loser-ns :played] inc)))

(defn record-match-result!
  [player1-ns player2-ns winner]
  (println (str "winner key is " winner))
  (cond
   (= :player1 winner) (do (update-winner! player1-ns)
                           (update-loser! player2-ns))
   (= :player2 winner) (do (update-winner! player2-ns)
                           (update-loser! player1-ns))))

(defn inc-match-count-if-below-limit!
  "Returns truthy if the number of matches was incremented, else falsey."
  [match-id limit]
  (let [current-match-tracker @match-tracker
        num-matches (current-match-tracker match-id)]
    (if (< num-matches limit)
      (compare-and-set! match-tracker current-match-tracker
                        (assoc current-match-tracker match-id (inc num-matches))))))

(defn play-match! [match-id limit]
  ;; check to see if we need to play the match by atomically checking the
  ;; number of matches against the limit
  (if (inc-match-count-if-below-limit! match-id limit)
    (let [[player1-ns player2-ns] (string/split match-id #"-")
          player-map @players
          player1 (player-map player1-ns)
          player2 (player-map player2-ns)]
      (println "----------------------------------------------")
      (let [{:keys [winner loser] :as game} (engine/play (:impl player1) (:impl player2))]
        (record-match-result! player1-ns player2-ns winner)
        (recur match-id limit)))))

(defn play-all-outstanding-games
  "Play all the matches up to the limit."
  [limit]
  (doseq [[match-id matches-played] @match-tracker]
    (play-match! match-id limit)))

(defn async-play-all-outstanding-games
  [limit]
  (.start (Thread. (partial play-all-outstanding-games limit))))

(defn player-exists? [name]
  (println @players-by-name)
  (@players-by-name name))

(defn register-player!
  [player player-ns]
  (let [registered-player (make-registered-player player player-ns)]
    (when-not (player-exists? (:name registered-player)) 
      (dosync
        (alter players assoc (str player-ns) registered-player)
        (alter players-by-name assoc (:name registered-player) registered-player)))
    (update-match-tracker! registered-player)))

;; always have the computer in here to play against people
(register-player! (core/make-random-cpu-player "cpu1") 'battleships.core)

(defn existing-player-id-matches? [name id]
  (if-let [existing-player (player-exists? name)]
    (do
      (println (str (:namespace existing-player) " matches " ))
      (= (symbol id) (:namespace existing-player)))))

(defn http-response 
  ([body]
    (http-response body 200))
  ([body status]
    (resp/status (resp/response body) status)))

(defn http-error
  [body]
  (http-response body 500))

(defn add-player
  [player-ns nm]
  (let [player (loader/make-player player-ns nm)]
    (register-player! player player-ns)
    (async-play-all-outstanding-games 10)
    (str player-ns)))

(defn create-player
  "Creates a new player, unless a player of that name already exists."
  [code nm]
  (if (player-exists? nm)
    (http-error (str "Failed: player " nm " already exists"))
    (if-let [player-ns (loader/eval-ns code)]
      (add-player player-ns nm)
      (http-error (str "Failed: player " nm " already exists")))))

(defn update-player
  "Attempts to update the player, failing if a player of that name doesn't
already exist, or the id doesn't match the player's id."
  [code nm id]
  (if (player-exists? nm)
    (if (existing-player-id-matches? nm id)
      (if-let [player-ns (loader/eval-ns code (symbol id))]
        (add-player player-ns nm))
      (http-error (str "Failed: id " id " does not match existing player's id")))
    (http-error (str "Failed: no player " nm " found to update."))))

;; These are the various pages the server provides.
(defroutes main-routes
  (GET "/" [] (view/main-page @players))
  (POST "/create" {:keys [body params] :as request} (create-player body
                                                            (:name params)))
  (POST "/update" {:keys [body params] :as request} (update-player body
                                                            (:name params)
                                                            (:id params)))
  (route/resources "/")
  (route/not-found "Page not found"))

(def handler
  (handler/site main-routes))
