(ns battleships.server-test
  (:use [battleships.server]
        [battleships.engine :as engine]
        [battleships.replay :as replay]
        [clojure.test]))

(deftest test-create-player
  (reset-state!)
  (let [player-id (create-player (java.io.File. "src/battleships/demo.clj") "Test")]
    (is (= 2 (num-players)))
    (is (= 2 (num-matchups)))
    (let [registered-players @players
          p1 (:impl (val (first registered-players)))
          p2 (:impl (val (second registered-players)))
          game (engine/play p1 p2)]
      (is (#{:player1 :player2} (:winner game)))
      (is (#{:player1 :player2} (:loser game)))
      (let [[p1 p2] (replay/build-players-from-game game)]
        (is (= game (engine/play p1 p2)))))))


