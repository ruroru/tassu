(ns jj.tassu
  (:require [clojure.string :as str]
            [clojure.tools.logging :as logger]))

(defrecord RadixNode [segment routes children])

(defn- parse-route-segment [segment]
  (if (str/starts-with? segment ":")
    {:type :param :name (keyword (subs segment 1))}
    {:type :literal :value segment}))

(defn- split-path [path]
  (->> (str/split path #"/")
       (remove empty?)))

(defn- insert-route [root path-segments route-handlers]
  (if (empty? path-segments)
    (assoc root :routes (concat (:routes root) route-handlers))

    (let [segment (first path-segments)
          remaining (rest path-segments)
          children (:children root)
          existing-child (first (filter #(= (:segment %) segment) children))
          child-node (if existing-child
                       existing-child
                       (->RadixNode segment [] []))
          updated-child (insert-route child-node remaining route-handlers)
          updated-children (if existing-child
                             (map #(if (= (:segment %) segment) updated-child %) children)
                             (conj children updated-child))]
      (assoc root :children updated-children))))

(defn- create-tree [router-spec]
  (reduce (fn [tree [path handlers]]
            (let [segments (split-path path)]
              (insert-route tree segments handlers)))
          (->RadixNode nil [] [])
          router-spec))

(defn- find-matching-node [node request-segments]
  (if (empty? request-segments)
    (when (seq (:routes node))
      {:node node :params {}})

    (let [current-segment (first request-segments)
          remaining-segments (rest request-segments)]

      (some (fn [child]
              (let [parsed (parse-route-segment (:segment child))]
                (case (:type parsed)
                  :literal
                  (when (= (:value parsed) current-segment)
                    (find-matching-node child remaining-segments))

                  :param
                  (when-let [result (find-matching-node child remaining-segments)]
                    (update result :params assoc (:name parsed) current-segment)))))
            (:children node)))))

(defn- find-matching-route [tree request-method request-path]
  (let [request-segments (split-path request-path)]
    (when-let [{:keys [node params]} (find-matching-node tree request-segments)]
      (some (fn [handler-spec]
              (when (= (:method handler-spec) request-method)
                {:handler (:handler handler-spec)
                 :params  params}))
            (:routes node)))))

(defn route
  [route-spec]
  (let [tree (create-tree route-spec)]
    (fn [request]
      (let [method (:request-method request)
            path (:uri request)
            match (find-matching-route tree method path)]
        (if match
          (let [enhanced-request (assoc request :params (:params match))]
            (when (logger/enabled? :debug)
              (logger/debugf "%s is handling %s" (:handler match) enhanced-request))
            ((:handler match) enhanced-request))
          {:status  404
           :body    "Not found"
           :headers {"content-type" "text/html"}})))))

(defn- create-route [method handler]
  {:method  method
   :handler handler})

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