# Tassu
Tassu is a ring router

## Installation
Add tassu to dependency list
```clojure
[org.clojars.jj/tassu "1.0.2"]
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