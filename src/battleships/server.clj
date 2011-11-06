(ns battleships.server
  (:use compojure.core)
  (:require [battleships.loader :as loader]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [ring.middleware.multipart-params :as mp]))


(defn upload
  [s]
  (if-let [player-ns (loader/eval-ns s)]
    (do
      (println player-ns)
      (println (loader/make-player player-ns))
      (str "Uploaded: " player-ns))
    ("Failed")))

(defroutes main-routes
  (GET "/" [] "<h1>Battleships</h1>")
  (POST "/upload" request (upload (:body request)))
  (route/resources "/")
  (route/not-found "Page not found"))

(def handler
  (handler/site main-routes))

