(ns battleships.server
  (:use compojure.core)
  (:require [battleships.loader :as loader]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [ring.util.response :as resp]))


(defn upload
  [s name]
  (if-let [player-ns (loader/eval-ns s)]
    (let [player (loader/make-player player-ns name)]
      (str player-ns))
    (resp/status "Upload failed" 500)))

(defroutes main-routes
  (GET "/" [] "<h1>Battleships</h1>")
  (POST "/upload" {:keys [body params] :as request} (upload body (:name params)))
  (route/resources "/")
  (route/not-found "Page not found"))

(def handler
  (handler/site main-routes))

