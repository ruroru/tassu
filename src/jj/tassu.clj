(ns jj.tassu
  (:require [clojure.string :as str]
            [clojure.tools.logging :as logger])
  (:import (java.util.regex Pattern)))

(defrecord RouteHandler [method handler])
(defrecord CompiledRouter [static-cache param-routes])
(defrecord ParamRoute [pattern param-keys method handler])

(defn- split-path [path]
  (->> (str/split path #"/")
       (remove empty?)
       vec))

(defn- compile-param-pattern [segments]
  (let [param-keys (atom [])
        pattern-parts (mapv (fn [segment]
                              (if (str/starts-with? segment ":")
                                (do
                                  (swap! param-keys conj (keyword (subs segment 1)))
                                  "([^/]+)")
                                (Pattern/quote segment)))
                            segments)]
    {:pattern    (re-pattern (str "^" (str/join "/" pattern-parts) "$"))
     :param-keys @param-keys}))

(defn- create-static-cache [route-specs]
  (reduce
    (fn [cache [path handlers]]
      (let [segments (split-path path)]
        (if (some #(str/starts-with? % ":") segments)
          cache
          (reduce
            (fn [acc handler]
              (assoc-in acc [path (:method handler)] (:handler handler)))
            cache
            handlers))))
    {}
    route-specs))

(defn- create-param-routes [route-specs]
  (reduce
    (fn [routes [path handlers]]
      (let [segments (split-path path)]
        (if (some #(str/starts-with? % ":") segments)
          (let [{:keys [pattern param-keys]} (compile-param-pattern segments)]
            (into routes
                  (map (fn [handler]
                         (->ParamRoute pattern param-keys (:method handler) (:handler handler)))
                       handlers)))
          routes)))
    []
    route-specs))

(defn- match-param-route [param-route uri-segments method]
  (when (= method (:method param-route))
    (let [path-str (str/join "/" uri-segments)
          matches (re-matches (:pattern param-route) path-str)]
      (when matches
        (let [param-values (rest matches)                   ; First match is the full string
              params (zipmap (:param-keys param-route) param-values)]
          {:handler (:handler param-route)
           :params  params})))))

(defn- find-param-route-match [param-routes uri-segments method]
  (some #(match-param-route % uri-segments method) param-routes))

(defn route
  [route-specs]
  (let [static-cache (create-static-cache route-specs)
        param-routes (create-param-routes route-specs)
        compiled-router (->CompiledRouter static-cache param-routes)]

    (fn [request]
      (let [method (:request-method request)
            uri (:uri request)
            static-match (get-in (:static-cache compiled-router) [uri method])
            param-match (when-not static-match
                          (let [uri-segments (split-path uri)]
                            (find-param-route-match (:param-routes compiled-router)
                                                    uri-segments
                                                    method)))]

        (cond
          static-match
          (let [enhanced-request (assoc request :params {})]
            (when (logger/enabled? :debug)
              (logger/debugf "Static handler %s handling %s" static-match enhanced-request))
            (static-match enhanced-request))

          param-match
          (let [enhanced-request (assoc request :params (:params param-match))]
            (when (logger/enabled? :debug)
              (logger/debugf "Param handler %s handling %s" (:handler param-match) enhanced-request))
            ((:handler param-match) enhanced-request))

          :else
          {:status  404
           :body    "Not found"
           :headers {"content-type" "text/html"}})))))

(defn- create-route [method handler]
  {:method method :handler handler})

(defn GET [handler]
  (create-route :get handler))

(defn HEAD [handler]
  (create-route :head handler))

(defn PUT [handler]
  (create-route :put handler))

(defn PATCH [handler]
  (create-route :patch handler))

(defn POST [handler]
  (create-route :post handler))

(defn DELETE [handler]
  (create-route :delete handler))