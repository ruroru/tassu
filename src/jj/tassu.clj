(ns jj.tassu
  (:require [clojure.string :as str])
  (:import (java.util.regex Pattern)))

(defrecord ParamRoute [pattern param-keys method handler specificity])

(defn- parse-path-segments
  [path]
  (loop [chars (seq path)
         current-segment []
         segments []]
    (cond
      (empty? chars)
      (if (empty? current-segment)
        segments
        (conj segments (apply str current-segment)))

      (= (first chars) \/)
      (if (empty? current-segment)
        (recur (rest chars) [] segments)
        (recur (rest chars) [] (conj segments (apply str current-segment))))

      :else
      (recur (rest chars) (conj current-segment (first chars)) segments))))

(defn- segment-starts-with-colon? [segment]
  (and (seq segment) (= (first segment) \:)))

(defn- extract-param-name [segment]
  (apply str (rest segment)))

(defn- route-specificity [segments]
  (reduce (fn [score segment]
            (if (segment-starts-with-colon? segment)
              score
              (+ score 10)))
          (count segments)
          segments))

(defn- compile-param-pattern [segments]
  (let [param-keys (transient [])
        pattern-parts (mapv (fn [segment]
                              (if (segment-starts-with-colon? segment)
                                (do
                                  (conj! param-keys (keyword (extract-param-name segment)))
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
      (let [segments (parse-path-segments path)]
        (if (some segment-starts-with-colon? segments)
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
                 (some segment-starts-with-colon? (parse-path-segments path))))
       (mapcat (fn [[path handlers]]
                 (let [segments (parse-path-segments path)
                       {:keys [pattern param-keys specificity]} (compile-param-pattern segments)]
                   (map (fn [handler]
                          (->ParamRoute pattern param-keys (:method handler)
                                        (:handler handler) specificity))
                        handlers))))
       (sort-by :specificity >)
       vec))

(defn- match-param-route [param-route uri method]
  (when (identical? method (:method param-route))
    (when-let [matches (re-matches (:pattern param-route) uri)]
      (let [param-values (rest matches)]
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