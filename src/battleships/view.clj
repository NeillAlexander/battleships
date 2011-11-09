(ns battleships.view
  (:use [hiccup.core]))

(defn make-player-table [players]
  (html [:table
         [:tr [:th "Name"] [:th "Played"] [:th "Won"] [:th "Lost"]]
         (for [{:keys [name played won lost]} (sort (fn [x y] (> (:won x) (:won y))) (vals players))]
           [:tr [:td name] [:td {:align "center"} played]
            [:td {:align "center"} won] [:td {:align "center"} lost]])]))

(defn main-page [players]
  (html [:h1 "Battleships"]
        (make-player-table players)))


