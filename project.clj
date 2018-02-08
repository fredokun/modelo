(defproject modelo "0.1.0-SNAPSHOT"
  :description "When formal methods meet Clojure (and vice-versa)"
  :url "https://github.com/fredokun/specl"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [clj-by-example "0.1.0-SNAPSHOT"]]
  :profiles {:dev {:plugins [[com.jakemccrary/lein-test-refresh "0.22.0"]]
                   :dependencies [[org.clojure/test.check "0.9.0"]]}})

