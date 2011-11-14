(ns battleships.client
  (:require [clojure.java.io :as io]
            [battleships.core :as core]
            [battleships.loader :as loader]
            [clj-http.client :as http]))


(defn test-player
  "player-ns is the path to the file that contains the namespace definition. This is for use when developing the player locally and want to make sure that it can play a game."
  [player-ns-file]
  (if-let [player-ns (loader/eval-ns player-ns-file)]
    (do
      (if (loader/valid-player-ns? player-ns)
        (core/test-player (loader/make-player player-ns))
        (println "invalid player"))
      (loader/cleanup-ns player-ns))))

(defn- post-to-server
  [player-ns-file name id server-address]
  (if (loader/valid-player-ns? (loader/eval-ns player-ns-file))
    (do
      (println (str "Submitting to " server-address))
      (http/post server-address {:body (slurp player-ns-file)
                                 :content-type "text/plain"
                                 :query-params {:name name :id id}}))))

(defn submit-player
  "Submit your player to the server."
  ([player-ns-file name]
     (submit-player player-ns-file name "http://localhost:3000/create"))
  ([player-ns-file name server-address]
     (post-to-server player-ns-file name nil server-address)))

(defn update-player
  "Use this when you want to overwrite your current player. You need the id returned from the original submission."
  ([player-ns-file name id]
     (update-player player-ns-file name id "http://localhost:3000/update"))
  ([player-ns-file name id server-address]
     (post-to-server player-ns-file name id server-address)))
