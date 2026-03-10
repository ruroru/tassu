(ns jj.async-tassu-test
  (:require [clojure.test :refer :all]
            [jj.tassu :as router :refer [GET HEAD PUT PATCH POST DELETE]])
  (:import (clojure.lang ExceptionInfo)))

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

(defn call-async
  "Calls an async handler and returns the response synchronously."
  [app req]
  (let [result (promise)]
    (app req
         (fn [res] (deliver result {:ok res}))
         (fn [err] (deliver result {:err err})))
    (let [{:keys [ok err]} (deref result 1000 {:err :timeout})]
      (if err
        (throw (ex-info "Async handler raised" {:cause err}))
        ok))))


(deftest router-not-found
  (let [app (router/async-route {})]
    (is (= default-not-found
           (call-async app (request "/"))))))

(deftest router-found-index
  (let [app (router/async-route {"/" [(GET (fn [req res rej]
                                             (res {:status 200 :body "get body"})))
                                      (PUT (fn [req res rej]
                                             (res {:status 200 :body "put body"})))]})]
    (is (= {:status 200 :body "put body"}
           (call-async app (request "/" :put))))
    (is (= {:status 200 :body "get body"}
           (call-async app (request "/"))))))

(deftest router-non-index-route
  (let [app (router/async-route {"/"            [(GET (fn [req res rej]
                                                        (res {:status 200 :body "index body"})))]
                                 "/api/v1/time" [(GET (fn [req res rej]
                                                        (res {:status 200 :body "api/v1/time body"})))]})]
    (is (= {:status 200 :body "index body"}
           (call-async app (request "/"))))
    (is (= {:status 200 :body "api/v1/time body"}
           (call-async app (request "/api/v1/time"))))))

(deftest parameterized-route
  (let [app (router/async-route {"/"                 [(GET (fn [req res rej]
                                                             (res {:status 200 :body "index body"})))]
                                 "/api/v1/users/:id" [(GET (fn [req res rej]
                                                             (is (= (:params req) {:id "admin"}))
                                                             (res {:status 200
                                                                   :body   (format "user is %s" (:id (:params req)))})))]})]
    (is (= {:status 200 :body "user is admin"}
           (call-async app (request "/api/v1/users/admin"))))))

(deftest multiple-parameters
  (let [app (router/async-route {"/users/:user-id/posts/:post-id"
                                 [(GET (fn [req res rej]
                                         (res {:status 200
                                               :body   (format "User %s, Post %s"
                                                               (:user-id (:params req))
                                                               (:post-id (:params req)))})))]})]
    (is (= {:status 200 :body "User john, Post 123"}
           (call-async app (request "/users/john/posts/123"))))))

(deftest route-priority
  (let [app (router/async-route {"/users/admin" [(GET (fn [req res rej]
                                                        (res {:status 200 :body "Admin user"})))]
                                 "/users/:id"   [(GET (fn [req res rej]
                                                        (res {:status 200 :body "Regular user"})))]})]
    (is (= {:status 200 :body "Admin user"}
           (call-async app (request "/users/admin"))))))

(deftest test-methods
  (let [app (router/async-route {"/" [(HEAD   (fn [req res rej] (res {:status 200 :body "head method"})))
                                      (POST   (fn [req res rej] (res {:status 200 :body "post method"})))
                                      (PUT    (fn [req res rej] (res {:status 200 :body "put method"})))
                                      (DELETE (fn [req res rej] (res {:status 200 :body "delete method"})))
                                      (PATCH  (fn [req res rej] (res {:status 200 :body "patch method"})))
                                      (GET    (fn [req res rej] (res {:status 200 :body "get method"})))]})]
    (are [expected method] (= expected (call-async app (request "/" method)))
                           {:status 200 :body "get method"}    :get
                           {:status 200 :body "head method"}   :head
                           {:status 200 :body "put method"}    :put
                           {:status 200 :body "patch method"}  :patch
                           {:status 200 :body "post method"}   :post
                           {:status 200 :body "delete method"} :delete)))

