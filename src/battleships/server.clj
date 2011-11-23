;; The server is what will run in the dojo for everyone to submit players to. It handles scheduling
;; games between all the submitted players, and displaying the results in a simple web-page.
(ns battleships.server
  (:use [compojure.core]
        [clojure.pprint])
  (:require [battleships.loader :as loader]
            [battleships.view :as view]
            [battleships.core :as core]
            [battleships.engine :as engine]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [ring.util.response :as resp]
            [clojure.string :as string]))

;; constant
;; note this means each will play each other 500 times as player1, and 500 times as player2
(def num-matches 500) 

;; Server state
(def players (ref {}))
(def players-by-name (ref {}))
(def match-tracker (atom {}))

(defn num-players [] (count @players))
(defn num-matchups [] (count @match-tracker))

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

(defn same-players? [player1 player2]
  (= (:namespace player1) (:namespace player2)))

;; When a new player is added we create a match up between it and all other players. This
;; is used to track and schedule all the required matches.
(defn create-match-up!
  ([player1 player2]
     (create-match-up! player1 player2 true))
  ([player1 player2 match1?]
     (let [match-id (str (:namespace player1) "-" (:namespace player2))]
       (swap! match-tracker assoc match-id (atom 0))
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

;; Kicks off a match using the engine.
(defn play-match! [match-id limit]
  ;; check to see if we need to play the match by atomically checking the
  ;; number of matches against the limit
  (if (inc-match-count-if-below-limit! match-id limit)
    (let [[player1-ns player2-ns] (string/split match-id #"-")
          player-map @players
          player1 (player-map player1-ns)
          player2 (player-map player2-ns)]
      (let [{:keys [winner loser] :as game} (engine/play (:impl player1) (:impl player2))]
        (record-match-result! player1-ns player2-ns winner)
        (recur match-id limit)))))

;; Triggered any time a new player is added.
(defn play-all-outstanding-games
  "Play all the matches up to the limit."
  [limit]
  (doseq [[match-id matches-played] @match-tracker]
    (play-match! match-id limit)))

(defn async-play-all-outstanding-games
  [limit]
  (.. (Thread. (partial play-all-outstanding-games limit)) start))

(defn player-exists? [name]
  (@players-by-name name))

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

(reset-state!)

(defn existing-player-id-matches? [name id]
  (if-let [existing-player (player-exists? name)]
    (= (symbol id) (:namespace existing-player))))

(defn http-response 
  ([body]
    (http-response body 200))
  ([body status]
    (resp/status (resp/response body) status)))

(defn http-error
  [body]
  (http-response body 500))

(defn add-player
  [player-ns nm code]
  (let [player (loader/make-player player-ns nm)]
    (register-player! player player-ns code)
    (async-play-all-outstanding-games num-matches)
    (str player-ns)))

;; This is used when a new player is added.
(defn create-player
  "Creates a new player, unless a player of that name already exists."
  [code nm]
  (if (player-exists? nm)
    (http-error (str "Failed: player " nm " already exists"))
    (if-let [ns-code (loader/read-ns code)] 
      (if-let [player-ns (loader/eval-ns ns-code)]
        (add-player player-ns nm ns-code)
        (http-error (str "Failed: player " nm " already exists")))
      (http-error (str "Failed: couldn't read player code")))))

;; When a player is updated i.e. the name matches and the correct namespace
;; was provided.
(defn update-player
  "Attempts to update the player, failing if a player of that name doesn't
already exist, or the id doesn't match the player's id."
  [code nm id]
  (if (player-exists? nm)
    (if (existing-player-id-matches? nm id)
      (if-let [ns-code (loader/read-ns code)]
        (if-let [player-ns (loader/eval-ns ns-code (symbol id))]
          (add-player player-ns nm ns-code))
        (http-error (str "Failed: error while loading code")))
      (http-error (str "Failed: id " id " does not match existing player's id")))
    (http-error (str "Failed: no player " nm " found to update."))))

(defn- local-request? [request]
  (= "127.0.0.1" (:remote-addr request)))

(defn view-player [name request]
  (if (local-request? request) 
    (if (player-exists? name)
      (view/make-player-view (@players-by-name name))
      (str name " not found"))))

;; These are the various pages the server provides.
(defroutes main-routes
  (GET "/" request (view/main-page @players (local-request? request)))
  (GET "/view" {:keys [params] :as request} (view-player (:name params) request))
  (POST "/create" {:keys [body params] :as request} (create-player body
                                                            (:name params)))
  (POST "/update" {:keys [body params] :as request} (update-player body
                                                            (:name params)
                                                            (:id params)))
  (route/resources "/")
  (route/not-found "Page not found"))

(def handler
  (handler/site main-routes))
