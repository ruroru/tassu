(defproject org.clojars.jj/tassu "1.0.1-SNAPSHOT"
  :description "A routing library for clojure ring"
  :url "https://github.com/ruroru/tassu"
  :license {:name "EPL-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.3"]]

  :deploy-repositories [["clojars" {:url      "https://repo.clojars.org"
                                    :username :env/clojars_user
                                    :password :env/clojars_pass}]]

  :profiles {:test {:dependencies [[hato "1.0.0"]
                                   [org.clojars.jj/ring-http-exchange "1.2.3"]]}}

  :plugins [[org.clojars.jj/bump "1.0.4"]
            [org.clojars.jj/strict-check "1.1.0"]
            [org.clojars.jj/bump-md "1.1.0"]])
