(ns battleships.server
  (:use compojure.core)
  (:require [battleships.loader :as loader]
            [battleships.view :as view]
            [battleships.core :as core]
            [battleships.engine :as engine]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [ring.util.response :as resp]))


(def players (atom {}))

(defn register-player!
  [player player-ns]
  (swap! players assoc player-ns
         {:name (engine/get-name player)
          :played 0
          :won 0
          :lost 0}))

;; always have the computer in here to play against people
(register-player! (core/make-random-cpu-player "cpu1") 'battleships.core)

(defn upload
  [s name]
  (if-let [player-ns (loader/eval-ns s)]
    (let [player (loader/make-player player-ns name)]
      (register-player! player player-ns)
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

