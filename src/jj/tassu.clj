(ns jj.tassu)

(deftype ParamRoute [segments param-keys method handler specificity])

(defn- fast-split
  [^String uri]
  (let [len (.length uri)]
    (if (or (<= len 1) (not (.startsWith uri "/")))
      []
      (loop [start 1
             res (transient [])]
        (let [end (.indexOf uri "/" start)]
          (if (== end -1)
            (persistent! (conj! res (.substring uri start len)))
            (recur (unchecked-inc end)
                   (conj! res (.substring uri start end)))))))))

(defn- segment-starts-with-colon? [^String segment]
  (and (not (.isEmpty segment)) (= (.charAt segment 0) \:)))

(defn- route-specificity [segments]
  (reduce (fn [score segment]
            (if (segment-starts-with-colon? segment)
              score
              (+ score 10)))
          (count segments)
          segments))

(defn- analyze-route-path [path]
  (let [raw-segments (fast-split path)
        param-keys (transient [])
        segments (mapv (fn [seg]
                         (if (segment-starts-with-colon? seg)
                           (do (conj! param-keys (keyword (.substring ^String seg 1)))
                               nil)
                           seg))
                       raw-segments)]
    {:segments    segments
     :param-keys  (persistent! param-keys)
     :specificity (route-specificity raw-segments)}))

(defn- create-static-cache [route-specs]
  (reduce
    (fn [cache [path handlers]]
      (let [segments (fast-split path)]
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
                 (some segment-starts-with-colon? (fast-split path))))
       (mapcat (fn [[path handlers]]
                 (let [{:keys [segments param-keys specificity]} (analyze-route-path path)]
                   (map (fn [handler]
                          (ParamRoute. segments param-keys (:method handler)
                                       (:handler handler) specificity))
                        handlers))))
       (sort-by (fn [^ParamRoute r] (.-specificity r)) >)
       vec))

(defn- match-param-route [^ParamRoute route uri-segments method]
  (let [r-segments (.-segments route)]
    (when (and (identical? method (.-method route))
               (== (count r-segments) (count uri-segments)))
      (let [p-keys (.-param-keys route)
            p-count (count p-keys)
            ^objects p-values (make-array Object p-count)]
        (loop [i 0
               pk-idx 0]
          (if (< i (count r-segments))
            (let [r-seg (nth r-segments i)
                  u-seg (nth uri-segments i)]
              (if (nil? r-seg)
                (do
                  (aset p-values pk-idx u-seg)
                  (recur (unchecked-inc i) (unchecked-inc pk-idx)))
                (if (.equals ^String r-seg u-seg)
                  (recur (unchecked-inc i) pk-idx)
                  nil)))
            (zipmap p-keys p-values)))))))

(defn- find-param-route-match [param-routes uri-segments method]
  (loop [routes param-routes]
    (if-let [route (first routes)]
      (if-let [params (match-param-route route uri-segments method)]
        {:handler (.-handler ^ParamRoute route) :params params}
        (recur (rest routes)))
      nil)))

(def ^:private not-found
  {:status  404
   :body    "Not found"
   :headers {"content-type" "text/html"}})

(defn route
  [route-specs]
  (let [static-cache (create-static-cache route-specs)
        param-routes (create-param-routes route-specs)]
    (fn [request]
      (let [method (:request-method request)
            uri (:uri request)]
        (if-let [static-handler (get-in static-cache [uri method])]
          (static-handler (assoc request :params {}))
          (let [uri-segments (fast-split uri)]
            (if-let [match (find-param-route-match param-routes uri-segments method)]
              ((:handler match) (assoc request :params (:params match)))
              not-found)))))))

(defn async-route
  [route-specs]
  (let [static-cache (create-static-cache route-specs)
        param-routes (create-param-routes route-specs)]
    (fn [request respond raise]
      (let [method (:request-method request)
            uri (:uri request)]
        (if-let [static-handler (get-in static-cache [uri method])]
          (static-handler (assoc request :params {}) respond raise)
          (let [uri-segments (fast-split uri)]
            (if-let [match (find-param-route-match param-routes uri-segments method)]
              ((:handler match) (assoc request :params (:params match)) respond raise)
              (respond not-found))))))))

(defn- create-route [method handler] {:method method :handler handler})
(defn GET [h] (create-route :get h))
(defn HEAD [h] (create-route :head h))
(defn PUT [h] (create-route :put h))
(defn PATCH [h] (create-route :patch h))
(defn POST [h] (create-route :post h))
(defn DELETE [h] (create-route :delete h))