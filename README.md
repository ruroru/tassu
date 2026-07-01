# Tassu
Tassu is a ring router

## Installation
Add tassu to dependency list
```clojure
[org.clojars.jj/tassu "1.0.4"]
```

## Usage

### Sync
```clojure
(ns jj
  (:require
    [jj.tassu :refer [route DELETE GET]]))

(def handler
  (route {"/"            [(GET (fn [req]
                                 {:status 200
                                  :body   "Foo"}))]
          "/get/version" [(GET (fn [req]
                                 {:status 200
                                  :body   "1.0.0"}))]
          "/users/:user" [(DELETE (fn [req]
                                    {:headers {"foo" "bar"}
                                     :status  201
                                     :body    "Baz"}))]}))
```
```clojure
(server/run-http-server handler {:port 8888})
```

### Async
```clojure
(ns jj
  (:require
    [jj.tassu :refer [async-route DELETE GET]]))

(def handler
  (async-route {"/"            [(GET (fn [req res rej]
                                       (res {:status 200
                                             :body   "Foo"})))]
                "/get/version" [(GET (fn [req res rej]
                                       (res {:status 200
                                             :body   "1.0.0"})))]
                "/users/:user" [(DELETE (fn [req res rej]
                                          (res {:headers {"foo" "bar"}
                                                :status  201
                                                :body    "Baz"})))]}))
```
```clojure
(server/run-http-server handler {:port 8888 :async? true})
```

Async handlers follow the standard Ring async signature `[request respond raise]`. Call `respond` with a response map on success, or `raise` with a `Throwable` on failure. The server adapter must support Ring async — Jetty, http-kit, and Aleph all do.

### Path parameters

Path segments starting with `:` are captured into `:params` as a map of keyword to string:

```clojure
(def handler
  (route {"/users/:user" [(GET (fn [req]
                                 {:status 200
                                  :body   (:user (:params req))}))]}))
```

### Query-param routing

Routes can also dispatch on query params by adding `:query-params` to an entry map:

```clojure
(def handler
  (route {"/products" [{:method       :get
                        :query-params "category=shoes&size=:size"
                        :handler      (fn [req]
                                        {:status 200
                                         :body   (:size (:query-params req))})}
                       {:method  :get
                        :handler (fn [req]
                                   {:status 200
                                    :body   "all products"})}]}))
```

- `category=shoes` requires `category` to be present with the exact value `shoes`.
- `size=:size` requires `size` to be present with any value.
- `verbose` (no `=`) requires the flag to be present; its value is `true`.

A request matches an entry when all of its required params are satisfied — extra params such as `utm_source` are tolerated. When several entries match, the most specific one wins: most required params first, then most exact values. If no entry matches, the entry without `:query-params` acts as a fallback; without one, the router responds 404.

Handlers receive the parsed query params in `:query-params` as a map of keyword to string. Repeated keys collect into a vector.

Both `route` and `async-route` accept an options map with `:before` and `:after` hooks:

```clojure
(def handler
  (route {"/users/:id" [(GET (fn [req]
                               {:status 200
                                :body   (:id (:params req))}))]}
         {:before (fn [req]
                    (assoc-in req [:headers "x-request-id"] (str (random-uuid))))
          :after  (fn [req response]
                    (assoc-in response [:headers "x-served-by"] "tassu"))}))
```

- `:before` takes the request and returns a (possibly modified) request. It runs before route matching, so it can rewrite `:uri` to affect routing.
- `:after` takes the request and the response, and returns a (possibly modified) response. It receives the request exactly as the matched handler saw it — including `:params` and `:query-params` — and runs on every response, including the built-in 404 (where no `:params` are present since no route matched). In `async-route`, `:after` wraps `respond`; errors delivered via `raise` bypass it.

## License

Copyright © 2025 [ruroru](https://github.com/ruroru)

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
https://www.eclipse.org/legal/epl-2.0/.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.