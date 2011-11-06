(ns battleships.client
  (:import [java.io PushbackReader]
           [battleships.engine ShipPosition])
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

(defn submit-player
  "Submit your player to the server."
  ([player-ns-file]
     (submit-player player-ns-file "http://localhost:3000/upload"))
  ([player-ns-file server-address]
     (println (str "Submitting to " server-address))
     (if (loader/valid-player-ns? (loader/eval-ns player-ns-file))       
       (http/post server-address {:body (slurp player-ns-file)
                                  :content-type "text/plain"}))))
