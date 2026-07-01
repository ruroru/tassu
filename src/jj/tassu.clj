(ns jj.tassu
  (:import (java.util HashMap)
           (java.net URLDecoder)))

(defn- method-idx ^long [method]
  (case method :get 0 :post 1 :put 2 :delete 3 :patch 4 :head 5 :options 6 -1))

(defn- parse-qs-keywords [^String qs]
  (when (and qs (not (.isEmpty qs)))
    (persistent!
      (reduce (fn [m ^String pair]
                (let [idx (.indexOf pair "=")
                      k (if (== idx -1)
                          (keyword (URLDecoder/decode pair "UTF-8"))
                          (keyword (URLDecoder/decode (.substring pair 0 idx) "UTF-8")))
                      v (if (== idx -1)
                          true
                          (URLDecoder/decode (.substring pair (inc idx)) "UTF-8"))
                      existing (get m k ::none)]
                  (assoc! m k (case existing
                                ::none v
                                (if (vector? existing)
                                  (conj existing v)
                                  [existing v])))))
              (transient {})
              (.split qs "&")))))

(defn- compile-qp-entry
  "Parse a query-param spec into [key-set exact-constraints]. Values starting
  with : are wildcards constrained only by key presence."
  [^String spec]
  (let [parsed (parse-qs-keywords spec)
        exact (into {} (remove (fn [[_ v]] (and (string? v) (.startsWith ^String v ":"))) parsed))]
    [(set (keys parsed)) exact]))

(defn- make-dispatcher
  "Given handlers for one method slot, returns a plain fn or a dispatcher map.
  Query-param candidates are subset matchers (extra request params are
  tolerated), ordered most specific first: most required keys, then most
  exact values."
  [entries]
  (let [with-qp (keep (fn [e]
                        (when-let [spec (:query-params e)]
                          (let [[key-set exact] (compile-qp-entry spec)]
                            {:key-set key-set :exact exact :handler (:handler e)})))
                      entries)
        fallback (:handler (first (remove :query-params entries)))]
    (if (empty? with-qp)
      fallback
      {:candidates (mapv (fn [{:keys [key-set exact handler]}] [key-set exact handler])
                         (sort-by (fn [{:keys [key-set exact]}]
                                    [(- (count key-set)) (- (count exact))])
                                  with-qp))
       :fallback   fallback})))

(deftype TrieNode [^HashMap children param-child param-key ^objects handlers param-builder])

(defn- strip-query-string
  ^String [^String uri]
  (let [idx (.indexOf uri "?")]
    (if (== idx -1) uri (.substring uri 0 idx))))

(defn- count-segments ^long [^String uri ^long len]
  (loop [i (int 1) n (int 0)]
    (if (< i len)
      (if (== (int (.charAt uri i)) (int 47))
        (recur (unchecked-inc-int i) n)
        (let [end (.indexOf uri (int 47) (unchecked-inc-int i))]
          (recur (int (if (== end -1) len end))
                 (unchecked-inc-int n))))
      n)))

(defn- fast-split-arr
  ^objects [^String uri]
  (let [len (.length uri)]
    (if (or (<= len 1) (not (.startsWith uri "/")))
      (object-array 0)
      (let [n (count-segments uri len)
            ^objects arr (make-array Object n)]
        (loop [start (int 1) idx (int 0)]
          (when (< idx n)
            (let [end (.indexOf uri (int 47) start)]
              (if (== end -1)
                (aset arr idx (.substring uri start len))
                (do (aset arr idx (.substring uri start end))
                    (recur (unchecked-inc-int end) (unchecked-inc-int idx)))))))
        arr))))

(defn- fast-split [^String uri]
  (let [len (.length uri)]
    (if (or (<= len 1) (not (.startsWith uri "/")))
      []
      (loop [start 1 res (transient [])]
        (let [end (.indexOf uri "/" start)]
          (if (== end -1)
            (persistent! (conj! res (.substring uri start len)))
            (recur (unchecked-inc end) (conj! res (.substring uri start end)))))))))

(defn- segment-starts-with-colon? [^String segment]
  (and (not (.isEmpty segment)) (= (.charAt segment 0) \:)))

(defn- new-mutable-node []
  {:children (HashMap.) :param-child (atom nil) :param-key (atom nil) :handlers (object-array 7)})

(defn- make-param-builder [param-keys]
  (let [ks (vec param-keys) n (count ks)]
    (case n
      0 (fn [^objects _] {})
      1 (let [k0 (nth ks 0)] (fn [^objects vs] {k0 (aget vs 0)}))
      2 (let [k0 (nth ks 0) k1 (nth ks 1)] (fn [^objects vs] {k0 (aget vs 0) k1 (aget vs 1)}))
      3 (let [k0 (nth ks 0) k1 (nth ks 1) k2 (nth ks 2)]
          (fn [^objects vs] {k0 (aget vs 0) k1 (aget vs 1) k2 (aget vs 2)}))
      (fn [^objects vs] (zipmap ks vs)))))

(defn- has-handlers? [^objects handlers]
  (loop [i 0]
    (if (< i 7)
      (if (aget handlers i) true (recur (unchecked-inc i)))
      false)))

(defn- freeze-node [mnode pkeys]
  (let [^HashMap src-children (:children mnode)
        ^HashMap frozen-children (HashMap. (.size src-children))
        pc @(:param-child mnode)
        pk @(:param-key mnode)
        ^objects handlers (:handlers mnode)
        builder (when (has-handlers? handlers) (make-param-builder pkeys))]
    (doseq [^java.util.Map$Entry e (.entrySet src-children)]
      (.put frozen-children (.getKey e) (freeze-node (.getValue e) pkeys)))
    (TrieNode. frozen-children
               (when pc (freeze-node pc (conj pkeys pk)))
               pk
               handlers
               builder)))

(defn- create-param-trie [route-specs]
  (let [mroot (new-mutable-node)
        param-paths (filter (fn [[path _]]
                              (some segment-starts-with-colon? (fast-split path)))
                            route-specs)]
    (doseq [[path handlers] param-paths
            :let [by-method (group-by :method handlers)]]
      (doseq [[method entries] by-method
              :let [midx (method-idx method)
                    dispatcher (make-dispatcher entries)]]
        (when (>= midx 0)
          (let [segs (fast-split path)
                leaf (reduce
                       (fn [node ^String seg]
                         (if (segment-starts-with-colon? seg)
                           (let [pk (keyword (.substring seg 1))]
                             (reset! (:param-key node) pk)
                             (or @(:param-child node)
                                 (let [child (new-mutable-node)]
                                   (reset! (:param-child node) child)
                                   child)))
                           (let [^HashMap children (:children node)]
                             (or (.get children seg)
                                 (let [child (new-mutable-node)]
                                   (.put children seg child)
                                   child)))))
                       mroot
                       segs)]
            (aset ^objects (:handlers leaf) midx dispatcher)))))
    (freeze-node mroot [])))

(defn- trie-match
  "Walk the trie. On match, returns [slot param-map]."
  [^TrieNode root ^objects uri-segments uri-count ^long midx]
  (let [^objects pvals (make-array Object 16)
        uri-count (int uri-count)]
    (loop [^TrieNode node root
           i (int 0)
           pk-idx (int 0)]
      (if (< i uri-count)
        (let [seg (aget uri-segments i)
              ^TrieNode literal (.get ^HashMap (.-children node) seg)]
          (if literal
            (recur literal (unchecked-inc-int i) pk-idx)
            (when-let [^TrieNode pc (.-param-child node)]
              (aset pvals pk-idx (URLDecoder/decode ^String seg "UTF-8"))
              (recur pc (unchecked-inc-int i) (unchecked-inc-int pk-idx)))))
        (when-let [slot (aget ^objects (.-handlers node) midx)]
          [slot ((.-param-builder node) pvals)])))))

(defn- create-static-cache [route-specs]
  (let [^HashMap cache (HashMap.)]
    (doseq [[path handlers] route-specs
            :let [segments (fast-split path)]
            :when (not (some segment-starts-with-colon? segments))]
      (let [^objects arr (or (.get cache path) (make-array Object 7))
            by-method (group-by :method handlers)]
        (doseq [[method entries] by-method
                :let [midx (method-idx method)]
                :when (>= midx 0)]
          (aset arr midx (make-dispatcher entries)))
        (.put cache path arr)))
    cache))

(defn- resolve-handler
  "Given a slot value (plain fn or dispatcher map), resolve to [handler query-params-map].
  The query string is parsed once; candidates are scanned most specific first,
  and a candidate matches when its required keys are present and its exact
  values agree — extra request params are tolerated."
  [slot request]
  (when slot
    (let [parsed (parse-qs-keywords (:query-string request))]
      (if (fn? slot)
        [slot parsed]
        (if-let [h (when parsed
                     (some (fn [[key-set exact handler]]
                             (when (and (every? (fn [k] (contains? parsed k)) key-set)
                                        (every? (fn [[k v]] (= v (get parsed k))) exact))
                               handler))
                           (:candidates slot)))]
          [h parsed]
          (when-let [fb (:fallback slot)] [fb parsed]))))))

(def ^:private not-found
  {:status 404 :body "Not found" :headers {"content-type" "text/html"}})

(def ^:private empty-params {})

(defn- apply-after [after request response]
  (if after (after request response) response))

(defn- wrap-respond [after request respond]
  (if after (fn [response] (respond (after request response))) respond))

(defn- compile-route
  [route-specs after]
  (let [^HashMap static-cache (create-static-cache route-specs)
        ^TrieNode trie (create-param-trie route-specs)]
    (fn [request]
      (let [method (:request-method request)
            ^String uri (strip-query-string (:uri request))
            midx (method-idx method)]
        (if-let [^objects methods (.get static-cache uri)]
          (if-let [[handler qp] (resolve-handler (aget methods midx) request)]
            (let [request (assoc request :params empty-params :query-params (or qp {}))]
              (apply-after after request (handler request)))
            (apply-after after request not-found))
          (let [^objects segs (fast-split-arr uri)
                result (trie-match trie segs (alength segs) midx)]
            (if result
              (let [[slot params] result
                    [handler qp] (resolve-handler slot request)]
                (if handler
                  (let [request (assoc request :params params :query-params (or qp {}))]
                    (apply-after after request (handler request)))
                  (apply-after after request not-found)))
              (apply-after after request not-found))))))))

(defn route
  ([route-specs] (route route-specs nil))
  ([route-specs {:keys [before after]}]
   (let [handler (compile-route route-specs after)]
     (if before
       (fn [request] (handler (before request)))
       handler))))

(defn- compile-async-route
  [route-specs after]
  (let [^HashMap static-cache (create-static-cache route-specs)
        ^TrieNode trie (create-param-trie route-specs)]
    (fn [request respond raise]
      (let [method (:request-method request)
            ^String uri (strip-query-string (:uri request))
            midx (method-idx method)]
        (if-let [^objects methods (.get static-cache uri)]
          (if-let [[handler qp] (resolve-handler (aget methods midx) request)]
            (let [request (assoc request :params empty-params :query-params (or qp {}))]
              (handler request (wrap-respond after request respond) raise))
            (respond (apply-after after request not-found)))
          (let [^objects segs (fast-split-arr uri)
                result (trie-match trie segs (alength segs) midx)]
            (if result
              (let [[slot params] result
                    [handler qp] (resolve-handler slot request)]
                (if handler
                  (let [request (assoc request :params params :query-params (or qp {}))]
                    (handler request (wrap-respond after request respond) raise))
                  (respond (apply-after after request not-found))))
              (respond (apply-after after request not-found)))))))))

(defn async-route
  ([route-specs] (async-route route-specs nil))
  ([route-specs {:keys [before after]}]
   (let [handler (compile-async-route route-specs after)]
     (if before
       (fn [request respond raise] (handler (before request) respond raise))
       handler))))

(defn- create-route [method handler] {:method method :handler handler})
(defn GET [h] (create-route :get h))
(defn HEAD [h] (create-route :head h))
(defn OPTIONS [h] (create-route :options h))
(defn PUT [h] (create-route :put h))
(defn PATCH [h] (create-route :patch h))
(defn POST [h] (create-route :post h))
(defn DELETE [h] (create-route :delete h))
