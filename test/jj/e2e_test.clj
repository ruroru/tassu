(ns jj.e2e-test
  (:require
    [clojure.test :refer [deftest is]]
    [hato.client :as hato]
    [jj.tassu :as router :refer [DELETE GET]]
    [ring-http-exchange.core :as server]))

(def handler
  (router/route {"/"             [(GET (fn [req]
                                         {:status 200
                                          :body   (format "%s request" (name (:request-method req)))}))]
                 "/get/version"  [(GET (fn [req]
                                         (is (= (:query-string req) "v=1.0.0"))
                                         {:status 200
                                          :body   (format "user is %s" (:id (:params req)))}))]
                 "/delete/:user" [(DELETE (fn [req]
                                            {:headers {"bar" "baz"}
                                             :status  201
                                             :body    (format "Deleted %s" (-> req
                                                                               :params
                                                                               :user))}))]}))

(deftest test-request
  (let [server (server/run-http-server handler {:port 8888})
        delete-response (hato/delete "http://localhost:8888/delete/user1")
        version-get-response (hato/get "http://localhost:8888/get/version?v=1.0.0")
        get-response (hato/get "http://localhost:8888/")]

    (is (= {"content-length" "11"} (dissoc (:headers get-response) "date")))
    (is (= 200 (:status get-response)))
    (is (= "get request" (:body get-response)))

    (is (= 201 (:status delete-response)))
    (is (= {"bar"            "baz"
            "content-length" "13"} (dissoc (:headers delete-response) "date")))
    (is (= "Deleted user1" (:body delete-response)))

    (is (= 200 (:status version-get-response)))

    (server/stop-http-server server)))