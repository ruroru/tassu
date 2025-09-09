(ns jj.tassu-test
  (:require [clojure.test :refer :all]
            [jj.tassu :as router :refer [GET]]))

(def default-not-found {:status  404
                        :body    "Not found"
                        :headers {"content-type" "text/html"}})

(defn request
  ([uri]
   (request uri :get))
  ([uri method]
   {:headers        {"key" "value"}
    :request-method method
    :uri            uri}))


(deftest router-not-found
  (let [app (router/route {})]
    (is (= default-not-found
           (app (request "/"))))))

(deftest router-found-index
  (let [app (router/route {"/" [{:method  :get
                                 :handler (fn [req]
                                            {:status 200
                                             :body   "get body"})}
                                {:method  :put
                                 :handler (fn [req]
                                            {:status 200
                                             :body   "put body"})}]})]
    (is (= {:status 200
            :body   "put body"}
           (app (request "/" :put))))
    (is (= {:status 200
            :body   "get body"}
           (app (request "/"))))))

(deftest router-non-index-route
  (let [app (router/route {"/"            [{:method  :get
                                            :handler (fn [req]
                                                       {:status 200
                                                        :body   "index body"})}]
                           "/api/v1/time" [{:method  :get
                                            :handler (fn [req]
                                                       {:status 200
                                                        :body   "api/v1/time body"})}]})]

    (is (= {:body   "index body"
            :status 200}
           (app (request "/"))))

    (is (= {:body   "api/v1/time body"
            :status 200}
           (app (request "/api/v1/time"))))))

(deftest parameterized-route
  (let [app (router/route {"/"                 [(GET (fn [req]
                                                       {:status 200
                                                        :body   "index body"}))]

                           "/api/v1/users/:id" [(GET (fn [req]
                                                       (is (= (:params req) {:id "admin"}))
                                                       {:status 200
                                                        :body   (format "user is %s" (:id (:params req)))}))]})]

    (is (= {:body   "user is admin"
            :status 200}
           (app (request "/api/v1/users/admin"))))))

(deftest multiple-parameters
  (let [app (router/route {"/users/:user-id/posts/:post-id"
                           [(GET (fn [req]
                                   {:status 200
                                    :body   (format "User %s, Post %s"
                                                    (:user-id (:params req))
                                                    (:post-id (:params req)))}))]})]
    (is (= {:status 200
            :body   "User john, Post 123"}
           (app (request "/users/john/posts/123"))))))

(deftest route-priority
  (let [app (router/route {"/users/admin"
                           [{:method  :get
                             :handler (fn [req] {:status 200 :body "Admin user"})}]
                           "/users/:id"
                           [{:method  :get
                             :handler (fn [req] {:status 200 :body "Regular user"})}]})]
    (is (= {:status 200 :body "Admin user"}
           (app (request "/users/admin"))))))