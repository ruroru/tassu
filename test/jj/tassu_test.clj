(ns jj.tassu-test
  (:require [clojure.test :refer :all]
            [jj.tassu :as router :refer [GET HEAD PUT PATCH POST DELETE]]))

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

(defn request-with-query-params
  ([uri query-params]
   (request-with-query-params uri :get query-params))
  ([uri method query-params]
   {:headers        {"key" "value"}
    :request-method method
    :query-string query-params
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


(deftest test-methods
  (let [app (router/route {
                           "/" [(HEAD (fn [req] {:status 200 :body "head method"}))
                                (POST (fn [req] {:status 200 :body "post method"}))
                                (PUT (fn [req] {:status 200 :body "put method"}))
                                (DELETE (fn [req] {:status 200 :body "delete method"}))
                                (PATCH (fn [req] {:status 200 :body "patch method"}))
                                (GET (fn [req] {:status 200 :body "get method"}))]})]
    (are [expected-response method] (= expected-response (app (request "/" method)))
                                    {:status 200 :body "get method"} :get
                                    {:status 200 :body "head method"} :head
                                    {:status 200 :body "put method"} :put
                                    {:status 200 :body "patch method"} :patch
                                    {:status 200 :body "post method"} :post
                                    {:status 200 :body "delete method"} :delete)))

(deftest query-params-routing
  (let [app (router/route {"/" [{:method       :get
                                 :query-params "category=shoes&size=10"
                                 :handler      (fn [req]
                                                 {:status 200
                                                  :body   "query-params-are-shoes-and-10"})}
                                {:method       :get
                                 :handler      (fn [req]
                                                 {:status 200
                                                  :body   "no-query-params"})}

                                {:method       :get
                                 :query-params "category=:category&size=:shoe-size"
                                 :handler      (fn [req]
                                                 {:status 200
                                                  :body   "query-params-are-shoes-and-10"})}
                                ]})]

    (testing "no query params behaves normally"
      (is (= {:status 200
              :body   "no-query-params"}
             (app (request "/" )))))

    (testing "Query param order does not matter"
      (is (= {:status 200
              :body   "query-params-are-shoes-and-10"}
             (app (request-with-query-params "/" "category=shoes&size=10"))))

      (is (= {:status 200
              :body   "query-params-are-shoes-and-10"}
             (app (request-with-query-params "/" "size=10&category=shoes")))))))

(deftest query-params-parsed-on-request
  (let [app (router/route {"/" [{:method       :get
                                 :query-params "name=alice&age=30"
                                 :handler      (fn [req]
                                                 (is (= {:name "alice" :age "30"} (:query-params req)))
                                                 {:status 200 :body "ok"})}]})]
    (is (= {:status 200 :body "ok"}
           (app (request-with-query-params "/" "age=30&name=alice"))))))

(deftest query-params-empty-when-no-qp-spec
  (let [app (router/route {"/" [{:method  :get
                                 :handler (fn [req]
                                            (is (= {} (:query-params req)))
                                            {:status 200 :body "ok"})}]})]
    (is (= {:status 200 :body "ok"}
           (app (request "/"))))))

(deftest query-params-no-match-falls-back
  (let [app (router/route {"/" [{:method       :get
                                 :query-params "foo=bar"
                                 :handler      (fn [req]
                                                 {:status 200 :body "matched-foo"})}
                                {:method  :get
                                 :handler (fn [req]
                                            (is (= {:baz "qux"} (:query-params req)))
                                            {:status 200 :body "fallback"})}]})]
    (testing "non-matching query params fall back to default handler"
      (is (= {:status 200 :body "fallback"}
             (app (request-with-query-params "/" "baz=qux")))))))

(deftest query-params-with-parameterized-values
  (let [app (router/route {"/" [{:method       :get
                                 :query-params "action=:action&id=:id"
                                 :handler      (fn [req]
                                                 (is (= {:action "delete" :id "42"} (:query-params req)))
                                                 {:status 200 :body "parameterized"})}
                                {:method  :get
                                 :handler (fn [req]
                                            {:status 200 :body "fallback"})}]})]
    (testing "parameterized query-params match any value"
      (is (= {:status 200 :body "parameterized"}
             (app (request-with-query-params "/" "action=delete&id=42")))))
    (testing "missing param key does not match"
      (is (= {:status 200 :body "fallback"}
             (app (request-with-query-params "/" "action=delete")))))))

(deftest query-params-different-methods
  (let [app (router/route {"/" [{:method       :get
                                 :query-params "v=1"
                                 :handler      (fn [req]
                                                 {:status 200
                                                  :body   "get-v1"})}
                                {:method       :post
                                 :query-params "v=1"
                                 :handler      (fn [req]
                                                 {:status 200
                                                  :body   "post-v1"})}
                                {:method  :get
                                 :handler (fn [req]
                                            {:status 200
                                             :body   "get-default"})}]})]
    (is (= {:status 200 :body "get-v1"}
           (app (request-with-query-params "/" :get "v=1"))))
    (is (= {:status 200 :body "post-v1"}
           (app (request-with-query-params "/" :post "v=1"))))
    (is (= {:status 200 :body "get-default"}
           (app (request "/"))))))

(deftest query-params-on-parameterized-routes
  (let [app (router/route {"/users/:id" [{:method       :get
                                          :query-params "detail=full"
                                          :handler      (fn [req]
                                                          (is (= "42" (:id (:params req))))
                                                          (is (= {:detail "full"} (:query-params req)))
                                                          {:status 200 :body "full-detail"})}
                                         {:method  :get
                                          :handler (fn [req]
                                                     (is (= "42" (:id (:params req))))
                                                     (is (= {} (:query-params req)))
                                                     {:status 200 :body "summary"})}]})]
    (is (= {:status 200 :body "full-detail"}
           (app (request-with-query-params "/users/42" "detail=full"))))
    (is (= {:status 200 :body "summary"}
           (app (request "/users/42"))))))


(deftest query-params-value-only
  (let [app (router/route {"/" [{:method       :get
                                 :query-params "verbose"
                                 :handler      (fn [req]
                                                 (is (= {:verbose true} (:query-params req)))
                                                 {:status 200 :body "verbose"})}
                                {:method  :get
                                 :handler (fn [req]
                                            {:status 200 :body "default"})}]})]
    (is (= {:status 200 :body "verbose"}
           (app (request-with-query-params "/" "verbose"))))
    (is (= {:status 200 :body "default"}
           (app (request "/"))))))

(deftest query-params-empty-value
  (let [app (router/route {"/" [{:method       :get
                                 :query-params "key="
                                 :handler      (fn [req]
                                                 (is (= {:key ""} (:query-params req)))
                                                 {:status 200 :body "empty-val"})}
                                {:method  :get
                                 :handler (fn [req]
                                            {:status 200 :body "default"})}]})]
    (is (= {:status 200 :body "empty-val"}
           (app (request-with-query-params "/" "key="))))
    (is (= {:status 200 :body "default"}
           (app (request "/"))))))

(deftest query-params-extra-params-tolerated
  (let [app (router/route {"/" [{:method       :get
                                 :query-params "category=shoes"
                                 :handler      (fn [req] {:status 200 :body "shoes"})}
                                {:method  :get
                                 :handler (fn [req] {:status 200 :body "fallback"})}]})]
    (testing "matching key set proceeds"
      (is (= {:status 200 :body "shoes"}
             (app (request-with-query-params "/" "category=shoes")))))
    (testing "extra params such as tracking noise still match"
      (is (= {:status 200 :body "shoes"}
             (app (request-with-query-params "/" "category=shoes&utm_source=x")))))))

(deftest query-params-more-required-keys-win
  (let [app (router/route {"/" [{:method       :get
                                 :query-params "a=1"
                                 :handler      (fn [req] {:status 200 :body "one-key"})}
                                {:method       :get
                                 :query-params "a=1&b=:b"
                                 :handler      (fn [req] {:status 200 :body "two-keys"})}]})]
    (testing "the candidate requiring more keys wins when both match"
      (is (= {:status 200 :body "two-keys"}
             (app (request-with-query-params "/" "a=1&b=2")))))
    (testing "the less specific candidate still matches on its own"
      (is (= {:status 200 :body "one-key"}
             (app (request-with-query-params "/" "a=1")))))))

(deftest query-params-specificity-order
  (let [app (router/route {"/" [{:method       :get
                                 :query-params "category=:c&size=:s"
                                 :handler      (fn [req] {:status 200 :body "wildcard"})}
                                {:method       :get
                                 :query-params "category=shoes&size=10"
                                 :handler      (fn [req] {:status 200 :body "exact"})}]})]
    (testing "exact values win over wildcards for the same key set"
      (is (= {:status 200 :body "exact"}
             (app (request-with-query-params "/" "category=shoes&size=10")))))
    (testing "non-matching values fall through to the wildcard candidate"
      (is (= {:status 200 :body "wildcard"}
             (app (request-with-query-params "/" "category=hats&size=9")))))))

(deftest query-params-overmatching
  (testing "request with more params than the spec still matches"
    (let [app (router/route {"/" [{:method       :get
                                   :query-params "a=1&b=:b"
                                   :handler      (fn [req] {:status 200 :body "spec"})}
                                  {:method  :get
                                   :handler (fn [req] {:status 200 :body "fallback"})}]})]
      (is (= {:status 200 :body "spec"}
             (app (request-with-query-params "/" "a=1&b=2"))))
      (is (= {:status 200 :body "spec"}
             (app (request-with-query-params "/" "a=1&b=2&c=3"))))))
  (testing "extra params match even without a fallback"
    (let [app (router/route {"/" [{:method       :get
                                   :query-params "a=1"
                                   :handler      (fn [req] {:status 200 :body "spec"})}]})]
      (is (= {:status 200 :body "spec"}
             (app (request-with-query-params "/" "a=1&b=2"))))))
  (testing "extra params do not rescue a failed value constraint"
    (let [app (router/route {"/" [{:method       :get
                                   :query-params "a=1"
                                   :handler      (fn [req] {:status 200 :body "spec"})}]})]
      (is (= default-not-found
             (app (request-with-query-params "/" "a=2&b=2")))))))

(deftest query-params-undermatching
  (testing "request with fewer params than the spec falls back"
    (let [app (router/route {"/" [{:method       :get
                                   :query-params "a=1&b=:b"
                                   :handler      (fn [req] {:status 200 :body "spec"})}
                                  {:method  :get
                                   :handler (fn [req] {:status 200 :body "fallback"})}]})]
      (is (= {:status 200 :body "fallback"}
             (app (request-with-query-params "/" "a=1"))))
      (is (= {:status 200 :body "fallback"}
             (app (request-with-query-params "/" "b=2"))))))
  (testing "request with fewer params and no fallback is not found"
    (let [app (router/route {"/" [{:method       :get
                                   :query-params "a=:a&b=:b"
                                   :handler      (fn [req] {:status 200 :body "spec"})}]})]
      (is (= default-not-found
             (app (request-with-query-params "/" "a=1"))))
      (is (= default-not-found
             (app (request "/")))))))

(deftest query-params-duplicate-keys
  (let [app (router/route {"/" [{:method  :get
                                 :handler (fn [req]
                                            {:status 200
                                             :body   (pr-str (:query-params req))})}]})]
    (is (= {:status 200 :body "{:a [\"1\" \"2\"]}"}
           (app (request-with-query-params "/" "a=1&a=2"))))))

(deftest before-hook-transforms-request
  (let [app (router/route {"/" [(GET (fn [req]
                                       {:status 200
                                        :body   (get-in req [:headers "x-added"])}))]}
                          {:before (fn [req] (assoc-in req [:headers "x-added"] "by-before"))})]
    (is (= {:status 200 :body "by-before"}
           (app (request "/"))))))

(deftest before-hook-can-rewrite-uri
  (let [app (router/route {"/v2/time" [(GET (fn [req] {:status 200 :body "v2 time"}))]}
                          {:before (fn [req] (update req :uri #(str "/v2" %)))})]
    (is (= {:status 200 :body "v2 time"}
           (app (request "/time"))))))

(deftest after-hook-transforms-response
  (let [app (router/route {"/" [(GET (fn [req] {:status 200 :body "ok"}))]}
                          {:after (fn [req response]
                                    (assoc-in response [:headers "x-uri"] (:uri req)))})]
    (is (= {:status 200 :body "ok" :headers {"x-uri" "/"}}
           (app (request "/"))))))

(deftest after-hook-runs-on-not-found
  (let [app (router/route {}
                          {:after (fn [req response] (assoc response :body "custom not found"))})]
    (is (= (assoc default-not-found :body "custom not found")
           (app (request "/missing"))))))

(deftest after-hook-sees-params-and-query-params
  (let [app (router/route {"/users/:id" [(GET (fn [req] {:status 200 :body "ok"}))]}
                          {:after (fn [req response]
                                    (is (= {:id "42"} (:params req)))
                                    (is (= {:detail "full"} (:query-params req)))
                                    response)})]
    (is (= {:status 200 :body "ok"}
           (app (request-with-query-params "/users/42" "detail=full"))))))

(deftest before-and-after-hooks-compose
  (let [app (router/route {"/" [(GET (fn [req]
                                       {:status 200
                                        :body   (get-in req [:headers "x-added"])}))]}
                          {:before (fn [req] (assoc-in req [:headers "x-added"] "by-before"))
                           :after  (fn [req response]
                                     (assoc-in response [:headers "x-echo"]
                                               (get-in req [:headers "x-added"])))})]
    (testing "after receives the request as transformed by before"
      (is (= {:status 200 :body "by-before" :headers {"x-echo" "by-before"}}
             (app (request "/")))))))

(deftest query-params-empty-query-string
  (let [app (router/route {"/" [{:method       :get
                                 :query-params "x=1"
                                 :handler      (fn [req]
                                                 {:status 200 :body "matched"})}
                                {:method  :get
                                 :handler (fn [req]
                                            {:status 200 :body "fallback"})}]})]
    (is (= {:status 200 :body "fallback"}
           (app {:request-method :get :uri "/" :query-string ""})))))
