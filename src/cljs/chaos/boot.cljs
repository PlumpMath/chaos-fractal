(ns chaos.boot
  (:require [chaos.fractal :as fractal :refer [chaos-game]]
            [chaos.input :as input]
            [chaos.util :refer [map-chan ch->seq]]            
            [cljs.core.async :as async :refer [>! <! chan timeout]]            
            [domina :as dom]
            [domina.css :as css]
            [clojure.string :as str])
  (:require-macros [cljs.core.async.macros :as m :refer [go alts! alt!]]))

(def initial-state {:shape (list [0 70.7] [-50 0] [50 0])
                    :ratio 0.5
                    :initial-position [0 0]
                    :next-position fractal/next-position})

(def render-options {:n 10000})

;; (declare build-controller)

(defn boot-chaos-app "in your browser..." [initial-state container]
  (let [result-container (make-div)
        controller (build-controller initial-state (make-div) result-container)]
    (dom/append! container controller)
    (dom/append! container result-container)))


(declare build-shape-input build-ratio-input build-go-button
         fancy-render-surface render-initial-state render-game)

(defn build-controller [initial container result-container]
  (let [{:keys [shape ratio]} initial]
    (go
     (while true
       (<! go-button-chan)
       (let [initial-state (<! input-state-chan)
             out-chan (chan)
             render-surface (fancy-render-surface)]
         (dom/append! result-container render-surface)
    
         (render-initial-state render-surface initial-state)
         (render-game render-surface render-options out-chan)

         (chaos-game input out-chan))))))

;; WIP

(def svg-ns "http://www.w3.org/2000/svg")

(defn attr-map->str [attrs]
  (str/join " "
            (map (fn [[k v]]
                   (str (name k) "=\"" v "\""))
                 attrs)))

(defn make-el
  ([el-name attrs] (make-el el-name attrs nil))
  ([el-name attrs inner]
     (let [el (.createElement js/document el-name)]
       (doseq [[k v] attrs]
         (.setAttribute el (name k) v))
       (set! (.-innerHTML el) inner)
       el)))

(defn make-el-ns [ns el-name attrs]
  (let [el (.createElementNS js/document ns el-name)]
    (doseq [[k v] attrs]
      (.setAttributeNS el nil (name k) v))
    el))


(def make-button (partial make-el "button"))
(def make-svg (partial make-el-ns svg-ns "svg" {}))
(def make-g (partial make-el-ns svg-ns "g"))
(def make-circle (partial make-el-ns svg-ns "circle"))

(defn render-game
  "A simple renderer"
  [render-surface options game-chan]
  (let [inner-surface (.createDocumentFragment js/document)]
    (go   
     (loop [i (options :n)]
       (when (> i 0)
         (let [p (<! game-chan)]
           (dom/append! inner-surface (make-circle
                                       {:cx (first p)
                                        :cy (second p)
                                        :r 0.1})))
         (recur (dec i))))

     (dom/append! render-surface inner-surface))))

(defn exponential-incremental-render-game [render-surface options game-chan]
  (let [{:keys [f interval max initial-n]} options
        initial-n (or initial-n 1)]
    (go
     (loop [n 1]
       (when (< n max)
         (render-game render-surface {:n n} game-chan)
         (when interval (<! (timeout interval)))
         (recur (* 2 n)))))))



(defn update-keys [rewrite-m m]
  (reduce (fn [m [k f]] (assoc m k (f (m k)))) m rewrite-m))


(defn rewrite-attribute-map [m]
  (partial update-keys {:transform rewrite-transform
                        :style rewrite-style}))

