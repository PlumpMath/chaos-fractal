(defproject chaos-fractal "0.1.0-SNAPSHOT"
  :description ""
  :url "http://lab.thisisacomputer.com/chaos-fractal"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-1835"]
                 [core.async "0.1.0-SNAPSHOT"]
                 [hiccup "1.0.3"]]

  :aliases {"brepl" ["trampoline" "cljsbuild" "repl-listen"]}

  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
  
  :plugins [[lein-cljsbuild "0.3.2"]]
  :source-paths ["src/clj"]  
  :hooks [leiningen.cljsbuild]

  :cljsbuild {:builds
              [{:source-paths ["src/cljs"]
                :compiler {:output-to "resources/public/js/surfsup.js"
                           :optimizations :whitespace}}]}

  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[com.cemerick/piggieback "0.0.5"]]}})
