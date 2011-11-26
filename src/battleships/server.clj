;; The server is what will run in the dojo for everyone to submit players to. It handles scheduling
;; games between all the submitted players, and displaying the results in a simple web-page.
(ns battleships.server
  (:use [compojure.core])
  (:require [battleships.loader :as loader]
            [battleships.view :as view]
            [battleships.core :as core]
            [battleships.engine :as engine]
            [battleships.server-state :as state]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [ring.util.response :as resp]
            [clojure.string :as string]))

;; constant
;; note this means each will play each other 500 times as player1, and 500 times as player2
(def ^{:private true} num-matches 500) 

;; Kicks off a match using the engine.
(defn- play-match [match-id limit]
  ;; check to see if we need to play the match by atomically checking the
  ;; number of matches against the limit
  (if (state/inc-match-count-if-below-limit! match-id limit)
    (let [[player1-ns player2-ns] (string/split match-id #"-")
          player-map (state/get-players)
          player1 (player-map player1-ns)
          player2 (player-map player2-ns)]
      (let [{:keys [winner loser] :as game} (engine/play (:impl player1) (:impl player2))]
        (state/record-match-result! player1-ns player2-ns winner)
        (recur match-id limit)))))

;; Triggered any time a new player is added.
(defn- play-all-outstanding-games
  "Play all the matches up to the limit."
  [limit]
  (doseq [[match-id matches-played] (state/get-match-tracker)]
    (play-match match-id limit)))

(defn- async-play-all-outstanding-games
  [limit]
  (.. (Thread. (partial play-all-outstanding-games limit)) start))

(defn- http-response 
  ([body]
    (http-response body 200))
  ([body status]
    (resp/status (resp/response body) status)))

(defn- http-error
  [body]
  (http-response body 500))

;; Registeres a player and schedules all the matches.
(defn- add-player
  [player-ns nm code]
  (let [player (loader/make-player player-ns nm)]
    (state/register-player! player player-ns code)
    (async-play-all-outstanding-games num-matches)
    (str player-ns)))

;; This is used when a new player is added.
(defn create-player
  "Creates a new player, unless a player of that name already exists."
  [code nm]
  (if (state/player-exists? nm)
    (http-error (str "Failed: player " nm " already exists"))
    (if-let [ns-code (loader/read-ns code)] 
      (if-let [player-ns (loader/eval-ns ns-code)]
        (add-player player-ns nm ns-code)
        (http-error (str "Failed: player " nm " already exists")))
      (http-error (str "Failed: couldn't read player code")))))

;; When a player is updated i.e. the name matches and the correct namespace
;; was provided.
(defn- update-player
  "Attempts to update the player, failing if a player of that name doesn't
already exist, or the id doesn't match the player's id."
  [code nm id]
  (if (state/player-exists? nm)
    (if (state/existing-player-id-matches? nm id)
      (if-let [ns-code (loader/read-ns code)]
        (if-let [player-ns (loader/eval-ns ns-code (symbol id))]
          (add-player player-ns nm ns-code))
        (http-error (str "Failed: error while loading code")))
      (http-error (str "Failed: id " id " does not match existing player's id")))
    (http-error (str "Failed: no player " nm " found to update."))))

;; Used to make sure can't snoop on other players code unless accessed from server.
(defn- local-request? [request]
  (= "127.0.0.1" (:remote-addr request)))

;; Creates the html by calling the view namespace.
(defn- view-player [name request]
  (if (local-request? request) 
    (if (state/player-exists? name)
      (view/make-player-view (state/get-player name))
      (str name " not found"))))

;; These are the various pages the server provides.
(defroutes main-routes
  (GET "/" request (view/main-page (state/get-players) (local-request? request)))
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

;; initialize state
(state/reset-state!)