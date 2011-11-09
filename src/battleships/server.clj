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

(defn update-winner! [winner-ns]
  (println (str "Winner is " winner-ns))
  (swap! players update-in [winner-ns :won] inc)
  (swap! players update-in [winner-ns :played] inc))

(defn update-loser! [loser-ns]
  (println (str "Loser is " loser-ns))
  (swap! players update-in [loser-ns :lost] inc)
  (swap! players update-in [loser-ns :played] inc))

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
        (println game)
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
      (async-play-all-outstanding-games 10)
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


