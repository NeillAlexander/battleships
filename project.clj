(defproject battleships "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [ring/ring-core "1.0.0-beta2"]
                 [ring/ring-jetty-adapter "1.0.0-beta2"]
                 [compojure "0.6.5"]
                 [clj-http "0.2.3"]
                 [hiccup "0.3.7"]]
  :dev-dependencies [[lein-ring "0.4.5"]]
  :main battleships.core
  :ring {:handler battleships.server/handler})
