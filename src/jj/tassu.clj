(ns jj.tassu
  (:require [clojure.string :as str])
  (:import (java.util.regex Pattern)))

(defrecord RouteHandler [method handler])
(defrecord CompiledRouter [static-cache param-routes])
(defrecord ParamRoute [pattern param-keys method handler specificity])

(defn- split-path [path]
  (->> (str/split path #"/")
       (remove empty?)
       vec))

(defn- route-specificity [segments]
  (reduce (fn [score segment]
            (if (str/starts-with? segment ":")
              score
              (+ score 10)))
          (count segments)
          segments))

(defn- compile-param-pattern [segments]
  (let [param-keys (transient [])
        pattern-parts (mapv (fn [segment]
                              (if (str/starts-with? segment ":")
                                (do
                                  (conj! param-keys (keyword (subs segment 1)))
                                  "([^/]+)")
                                (Pattern/quote segment)))
                            segments)
        pattern-str (if (empty? pattern-parts)
                      "^/$"
                      (str "^/" (str/join "/" pattern-parts) "$"))]
    {:pattern     (re-pattern pattern-str)
     :param-keys  (persistent! param-keys)
     :specificity (route-specificity segments)}))

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
  (->> route-specs
       (filter (fn [[path _]]
                 (some #(str/starts-with? % ":") (split-path path))))
       (mapcat (fn [[path handlers]]
                 (let [segments (split-path path)
                       {:keys [pattern param-keys specificity]} (compile-param-pattern segments)]
                   (map (fn [handler]
                          (->ParamRoute pattern param-keys (:method handler)
                                        (:handler handler) specificity))
                        handlers))))
       (sort-by :specificity >)                             ; sort by specificity descending
       vec))

(defn- match-param-route [param-route uri method]
  (when (identical? method (:method param-route))
    (when-let [matches (re-matches (:pattern param-route) uri)]
      (let [param-values (rest matches)]                    ; skip full match
        {:handler (:handler param-route)
         :params  (zipmap (:param-keys param-route) param-values)}))))

(defn- find-param-route-match [param-routes uri method]
  (loop [routes param-routes]
    (when-let [route (first routes)]
      (if-let [match (match-param-route route uri method)]
        match
        (recur (rest routes))))))

(defn route
  [route-specs]
  (let [static-cache (create-static-cache route-specs)
        param-routes (create-param-routes route-specs)]

    (fn [request]
      (let [method (:request-method request)
            uri (:uri request)]

        (if-let [static-handler (get-in static-cache [uri method])]
          (static-handler (assoc request :params {}))

          (if-let [param-match (find-param-route-match param-routes uri method)]
            ((:handler param-match) (assoc request :params (:params param-match)))

            {:status  404
             :body    "Not found"
             :headers {"content-type" "text/html"}}))))))

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