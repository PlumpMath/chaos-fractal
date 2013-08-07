(ns dev
  (:require [cljs.repl :as repl]
            [cljs.repl.browser :as browser]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.adapter.jetty :as jetty]
            [cemerick.piggieback :as piggieback]
            [hiccup.core :as hiccup]))

(def handler
  (-> (fn [request] (throw (ex-info "Not found" request)))
      (wrap-resource "public/")
      wrap-content-type))

(defn reset-server [context]
  (when-let [server (:server context)]
    (.stop server))
  (assoc context :server
         (jetty/run-jetty handler {:port 8080 :join? false})))

(defn make-browser-repl-env []
  (doto (browser/repl-env :port 9000) repl/-setup))

(defn reset-repl [context]
  (when-let [repl (:repl context)]
    ;; -tear-down can fail if the repl has already been torn down
    (try (repl/-tear-down (:repl context))
         (catch Throwable e nil)))
  (assoc context :repl (make-browser-repl-env)))

(defn stop [context]
  (.stop (:server context))
  (reset-repl context))

(defonce dev-context nil)

(defn dev-start []
  (alter-var-root #'dev-context
                  (constantly (-> dev-context
                                  reset-server
                                  reset-repl))))

(defn dev-piggieback []
  (piggieback/cljs-repl :repl-env (dev-context :repl)))

