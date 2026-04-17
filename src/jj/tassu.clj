(ns jj.tassu
  (:import (java.util HashMap)))

(deftype WildcardRoute [^String prefix ^long prefix-len method handler])

(defn- method-idx ^long [method]
  (case method :get 0 :post 1 :put 2 :delete 3 :patch 4 :head 5 :options 6 -1))

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

(defn- mutable-trie-insert [root segments method handler]
  (let [leaf (reduce
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
               root
               segments)]
    (aset ^objects (:handlers leaf) (method-idx method) handler)))

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
            handler handlers]
      (mutable-trie-insert mroot (fast-split path) (:method handler) (:handler handler)))
    (freeze-node mroot [])))

(defn- trie-match
  "Walk the trie. On match, calls (found handler params). On miss, returns nil."
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
              (aset pvals pk-idx seg)
              (recur pc (unchecked-inc-int i) (unchecked-inc-int pk-idx)))))
        (when-let [handler (aget ^objects (.-handlers node) midx)]
          [handler ((.-param-builder node) pvals)])))))

(defn- create-static-cache [route-specs]
  (let [^HashMap cache (HashMap.)]
    (doseq [[path handlers] route-specs
            :let [segments (fast-split path)]
            :when (not (some segment-starts-with-colon? segments))
            :when (not (.endsWith ^String path "/*"))]
      (let [^objects arr (or (.get cache path) (make-array Object 7))]
        (doseq [handler handlers
                :let [midx (method-idx (:method handler))]
                :when (>= midx 0)]
          (aset arr midx (:handler handler)))
        (.put cache path arr)))
    cache))

(defn- create-wildcard-routes [route-specs]
  (->> route-specs
       (filter (fn [[path _]] (.endsWith ^String path "/*")))
       (mapcat (fn [[path handlers]]
                 (let [^String prefix (.substring ^String path 0 (- (count path) 1))]
                   (map (fn [handler]
                          (WildcardRoute. prefix (.length prefix) (:method handler) (:handler handler)))
                        handlers))))
       (into-array WildcardRoute)))

(defn- find-wildcard-match [^"[Ljj.tassu.WildcardRoute;" wildcard-routes ^String uri method]
  (let [n (alength wildcard-routes)]
    (loop [i (int 0)]
      (when (< i n)
        (let [^WildcardRoute route (aget wildcard-routes i)]
          (if (and (= method (.-method route))
                   (.startsWith uri (.-prefix route)))
            [(.-handler route) {:* (.substring uri (.-prefix-len route))}]
            (recur (unchecked-inc-int i))))))))

(def ^:private not-found
  {:status 404 :body "Not found" :headers {"content-type" "text/html"}})

(def ^:private empty-params {})

(defn route
  [route-specs]
  (let [^HashMap static-cache (create-static-cache route-specs)
        ^TrieNode trie (create-param-trie route-specs)
        wildcard-routes (create-wildcard-routes route-specs)]
    (fn [request]
      (let [method (:request-method request)
            ^String uri (strip-query-string (:uri request))
            midx (method-idx method)]
        (if-let [^objects methods (.get static-cache uri)]
          (if-let [handler (aget methods midx)]
            (handler (assoc request :params empty-params))
            not-found)
          (let [^objects segs (fast-split-arr uri)
                result (trie-match trie segs (alength segs) midx)]
            (if result
              ((nth result 0) (assoc request :params (nth result 1)))
              (if-let [wm (find-wildcard-match wildcard-routes uri method)]
                ((nth wm 0) (assoc request :params (nth wm 1)))
                not-found))))))))

(defn async-route
  [route-specs]
  (let [^HashMap static-cache (create-static-cache route-specs)
        ^TrieNode trie (create-param-trie route-specs)
        wildcard-routes (create-wildcard-routes route-specs)]
    (fn [request respond raise]
      (let [method (:request-method request)
            ^String uri (strip-query-string (:uri request))
            midx (method-idx method)]
        (if-let [^objects methods (.get static-cache uri)]
          (if-let [handler (aget methods midx)]
            (handler (assoc request :params empty-params) respond raise)
            (respond not-found))
          (let [^objects segs (fast-split-arr uri)
                result (trie-match trie segs (alength segs) midx)]
            (if result
              ((nth result 0) (assoc request :params (nth result 1)) respond raise)
              (if-let [wm (find-wildcard-match wildcard-routes uri method)]
                ((nth wm 0) (assoc request :params (nth wm 1)) respond raise)
                (respond not-found)))))))))

(defn- create-route [method handler] {:method method :handler handler})
(defn GET [h] (create-route :get h))
(defn HEAD [h] (create-route :head h))
(defn OPTIONS [h] (create-route :options h))
(defn PUT [h] (create-route :put h))
(defn PATCH [h] (create-route :patch h))
(defn POST [h] (create-route :post h))
(defn DELETE [h] (create-route :delete h))
