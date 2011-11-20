(ns battleships.view
  (:use [hiccup.core]))

(defn make-player-table [players]
  (html [:table
         [:tr [:th "Name"] [:th "Played"] [:th "Won"] [:th "Lost"]]
         (for [{:keys [name played won lost]} (sort (fn [x y] (> (:won x) (:won y))) (vals players))]
           [:tr [:td [:a {:href (str "/view?name="name)} name]] [:td {:align "center"} played]
            [:td {:align "center"} won] [:td {:align "center"} lost]])]))

(defn make-player-view [registered-player]
  (html [:h2 (:name registered-player) ]
        [:div "Created: " (:created registered-player)]
        [:div "Updated: " (:updated registered-player)]
        [:pre (:code registered-player)]))

(defn main-page [players]
  (html
    [:head [:meta {:HTTP-EQUIV "Refresh" :CONTENT "1"}]]
    [:body 
     [:h1 "Battleships"]
     (make-player-table players)]))


