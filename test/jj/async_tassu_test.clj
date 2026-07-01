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


(defn request-with-query-params
  ([uri query-params]
   (request-with-query-params uri :get query-params))
  ([uri method query-params]
   {:headers        {"key" "value"}
    :request-method method
    :query-string   query-params
    :uri            uri}))

(deftest query-params-routing
  (let [app (router/async-route {"/" [{:method       :get
                                       :query-params "category=shoes&size=10"
                                       :handler      (fn [req res rej]
                                                       (res {:status 200 :body "matched"}))}
                                      {:method  :get
                                       :handler (fn [req res rej]
                                                  (res {:status 200 :body "no-query-params"}))}]})]
    (testing "no query params behaves normally"
      (is (= {:status 200 :body "no-query-params"}
             (call-async app (request "/")))))
    (testing "Query param order does not matter"
      (is (= {:status 200 :body "matched"}
             (call-async app (request-with-query-params "/" "category=shoes&size=10"))))
      (is (= {:status 200 :body "matched"}
             (call-async app (request-with-query-params "/" "size=10&category=shoes")))))))

(deftest async-query-params-parsed-on-request
  (let [app (router/async-route {"/" [{:method       :get
                                       :query-params "name=alice&age=30"
                                       :handler      (fn [req res rej]
                                                       (is (= {:name "alice" :age "30"} (:query-params req)))
                                                       (res {:status 200 :body "ok"}))}]})]
    (is (= {:status 200 :body "ok"}
           (call-async app (request-with-query-params "/" "age=30&name=alice"))))))

(deftest async-query-params-empty-when-no-qp-spec
  (let [app (router/async-route {"/" [{:method  :get
                                       :handler (fn [req res rej]
                                                  (is (= {} (:query-params req)))
                                                  (res {:status 200 :body "ok"}))}]})]
    (is (= {:status 200 :body "ok"}
           (call-async app (request "/"))))))

(deftest async-query-params-no-match-falls-back
  (let [app (router/async-route {"/" [{:method       :get
                                       :query-params "foo=bar"
                                       :handler      (fn [req res rej]
                                                       (res {:status 200 :body "matched-foo"}))}
                                      {:method  :get
                                       :handler (fn [req res rej]
                                                  (is (= {:baz "qux"} (:query-params req)))
                                                  (res {:status 200 :body "fallback"}))}]})]
    (is (= {:status 200 :body "fallback"}
           (call-async app (request-with-query-params "/" "baz=qux"))))))

(deftest async-query-params-with-parameterized-values
  (let [app (router/async-route {"/" [{:method       :get
                                       :query-params "action=:action&id=:id"
                                       :handler      (fn [req res rej]
                                                       (is (= {:action "delete" :id "42"} (:query-params req)))
                                                       (res {:status 200 :body "parameterized"}))}
                                      {:method  :get
                                       :handler (fn [req res rej]
                                                  (res {:status 200 :body "fallback"}))}]})]
    (is (= {:status 200 :body "parameterized"}
           (call-async app (request-with-query-params "/" "action=delete&id=42"))))
    (is (= {:status 200 :body "fallback"}
           (call-async app (request-with-query-params "/" "action=delete"))))))

(deftest async-query-params-on-parameterized-routes
  (let [app (router/async-route {"/users/:id" [{:method       :get
                                                :query-params "detail=full"
                                                :handler      (fn [req res rej]
                                                                (is (= "42" (:id (:params req))))
                                                                (is (= {:detail "full"} (:query-params req)))
                                                                (res {:status 200 :body "full-detail"}))}
                                               {:method  :get
                                                :handler (fn [req res rej]
                                                           (is (= "42" (:id (:params req))))
                                                           (is (= {} (:query-params req)))
                                                           (res {:status 200 :body "summary"}))}]})]
    (is (= {:status 200 :body "full-detail"}
           (call-async app (request-with-query-params "/users/42" "detail=full"))))
    (is (= {:status 200 :body "summary"}
           (call-async app (request "/users/42"))))))

(deftest async-query-params-value-only
  (let [app (router/async-route {"/" [{:method       :get
                                       :query-params "verbose"
                                       :handler      (fn [req res rej]
                                                       (is (= {:verbose true} (:query-params req)))
                                                       (res {:status 200 :body "verbose"}))}
                                      {:method  :get
                                       :handler (fn [req res rej]
                                                  (res {:status 200 :body "default"}))}]})]
    (is (= {:status 200 :body "verbose"}
           (call-async app (request-with-query-params "/" "verbose"))))
    (is (= {:status 200 :body "default"}
           (call-async app (request "/"))))))

(deftest async-query-params-empty-value
  (let [app (router/async-route {"/" [{:method       :get
                                       :query-params "key="
                                       :handler      (fn [req res rej]
                                                       (is (= {:key ""} (:query-params req)))
                                                       (res {:status 200 :body "empty-val"}))}
                                      {:method  :get
                                       :handler (fn [req res rej]
                                                  (res {:status 200 :body "default"}))}]})]
    (is (= {:status 200 :body "empty-val"}
           (call-async app (request-with-query-params "/" "key="))))
    (is (= {:status 200 :body "default"}
           (call-async app (request "/"))))))

(deftest async-query-params-overmatching
  (testing "request with more params than the spec still matches"
    (let [app (router/async-route {"/" [{:method       :get
                                         :query-params "a=1&b=:b"
                                         :handler      (fn [req res rej]
                                                         (res {:status 200 :body "spec"}))}
                                        {:method  :get
                                         :handler (fn [req res rej]
                                                    (res {:status 200 :body "fallback"}))}]})]
      (is (= {:status 200 :body "spec"}
             (call-async app (request-with-query-params "/" "a=1&b=2"))))
      (is (= {:status 200 :body "spec"}
             (call-async app (request-with-query-params "/" "a=1&b=2&c=3"))))))
  (testing "extra params match even without a fallback"
    (let [app (router/async-route {"/" [{:method       :get
                                         :query-params "a=1"
                                         :handler      (fn [req res rej]
                                                         (res {:status 200 :body "spec"}))}]})]
      (is (= {:status 200 :body "spec"}
             (call-async app (request-with-query-params "/" "a=1&b=2"))))))
  (testing "extra params do not rescue a failed value constraint"
    (let [app (router/async-route {"/" [{:method       :get
                                         :query-params "a=1"
                                         :handler      (fn [req res rej]
                                                         (res {:status 200 :body "spec"}))}]})]
      (is (= default-not-found
             (call-async app (request-with-query-params "/" "a=2&b=2")))))))

(deftest async-query-params-undermatching
  (testing "request with fewer params than the spec falls back"
    (let [app (router/async-route {"/" [{:method       :get
                                         :query-params "a=1&b=:b"
                                         :handler      (fn [req res rej]
                                                         (res {:status 200 :body "spec"}))}
                                        {:method  :get
                                         :handler (fn [req res rej]
                                                    (res {:status 200 :body "fallback"}))}]})]
      (is (= {:status 200 :body "fallback"}
             (call-async app (request-with-query-params "/" "a=1"))))
      (is (= {:status 200 :body "fallback"}
             (call-async app (request-with-query-params "/" "b=2"))))))
  (testing "request with fewer params and no fallback is not found"
    (let [app (router/async-route {"/" [{:method       :get
                                         :query-params "a=:a&b=:b"
                                         :handler      (fn [req res rej]
                                                         (res {:status 200 :body "spec"}))}]})]
      (is (= default-not-found
             (call-async app (request-with-query-params "/" "a=1"))))
      (is (= default-not-found
             (call-async app (request "/")))))))

(deftest async-before-hook-transforms-request
  (let [app (router/async-route {"/" [(GET (fn [req res rej]
                                             (res {:status 200
                                                   :body   (get-in req [:headers "x-added"])})))]}
                                {:before (fn [req] (assoc-in req [:headers "x-added"] "by-before"))})]
    (is (= {:status 200 :body "by-before"}
           (call-async app (request "/"))))))

(deftest async-after-hook-transforms-response
  (let [app (router/async-route {"/" [(GET (fn [req res rej]
                                             (res {:status 200 :body "ok"})))]}
                                {:after (fn [req response]
                                          (assoc-in response [:headers "x-uri"] (:uri req)))})]
    (is (= {:status 200 :body "ok" :headers {"x-uri" "/"}}
           (call-async app (request "/"))))))

(deftest async-after-hook-runs-on-not-found
  (let [app (router/async-route {}
                                {:after (fn [req response] (assoc response :body "custom not found"))})]
    (is (= (assoc default-not-found :body "custom not found")
           (call-async app (request "/missing"))))))

(deftest async-after-hook-sees-params-and-query-params
  (let [app (router/async-route {"/users/:id" [(GET (fn [req res rej]
                                                      (res {:status 200 :body "ok"})))]}
                                {:after (fn [req response]
                                          (is (= {:id "42"} (:params req)))
                                          (is (= {:detail "full"} (:query-params req)))
                                          response)})]
    (is (= {:status 200 :body "ok"}
           (call-async app (request-with-query-params "/users/42" "detail=full"))))))

(deftest async-before-and-after-hooks-compose
  (let [app (router/async-route {"/" [(GET (fn [req res rej]
                                             (res {:status 200
                                                   :body   (get-in req [:headers "x-added"])})))]}
                                {:before (fn [req] (assoc-in req [:headers "x-added"] "by-before"))
                                 :after  (fn [req response]
                                           (assoc-in response [:headers "x-echo"]
                                                     (get-in req [:headers "x-added"])))})]
    (is (= {:status 200 :body "by-before" :headers {"x-echo" "by-before"}}
           (call-async app (request "/"))))))

(deftest async-raise-bypasses-after-hook
  (let [app (router/async-route {"/" [(GET (fn [req res rej]
                                             (rej (ex-info "boom" {}))))]}
                                {:after (fn [req response] (assoc response :body "should not run"))})]
    (is (thrown? ExceptionInfo (call-async app (request "/"))))))

(deftest async-query-params-duplicate-keys
  (let [app (router/async-route {"/" [{:method  :get
                                       :handler (fn [req res rej]
                                                  (res {:status 200
                                                        :body   (pr-str (:query-params req))}))}]})]
    (is (= {:status 200 :body "{:a [\"1\" \"2\"]}"}
           (call-async app (request-with-query-params "/" "a=1&a=2"))))))
