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


(def players (atom {}))
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

(defn play-match! [match limit]
  (let [[player1-ns player2-ns] (string/split (key match) #"-")
        num-games (val match)
        player-map @players
        player1 (player-map player1-ns)
        player2 (player-map player2-ns)]
    (println (str "Ready to play match between " player1-ns " and " player2-ns " " num-games))
    (let [{:keys [winner loser]} (engine/play (:impl player1) (:impl player2))]
      (println winner))))

(defn play-all-outstanding-games
  "Play all the matches up to the limit."
  [limit]
  (doseq [match @match-tracker :when (< (val match) limit)]
    (play-match! match limit)))

(defn register-player!
  [player player-ns]  
  (let [registered-player (make-registered-player player player-ns)]
    (swap! players assoc (str player-ns) registered-player)
    (update-match-tracker! registered-player)))

;; always have the computer in here to play against people
(register-player! (core/make-random-cpu-player "cpu1") 'battleships.core)

(defn upload
  [s name]
  (if-let [player-ns (loader/eval-ns s)]
    (let [player (loader/make-player player-ns name)]
      (register-player! player player-ns)
      (play-all-outstanding-games 10)
      (str player-ns))
    (resp/status "Upload failed" 500)))

;; These are the various pages the server provides.
(defroutes main-routes
  (GET "/" [] (view/main-page @players))
  (POST "/upload" {:keys [body params] :as request} (upload body (:name params)))
  (route/resources "/")
  (route/not-found "Page not found"))

(def handler
  (handler/site main-routes))


