(ns chaos.boot
  (:require [chaos.fractal :as fractal :refer [chaos-game]]
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

(defn boot-chaos-app "in your browser..." [container]
  (let [result-container (make-div)
        controller (build-controller initial-state (make-div) result-container)]
    (dom/append! container controller)
    (dom/append! container result-container)))


(declare build-shape-input build-ratio-input build-go-button
         fancy-render-surface render-initial-state render-game)

(defn build-controller [initial container result-container]
  (let [{:keys [shape ratio]} initial

        shape-input-chan (chan)
        shape-input (build-shape-input point-input-chan)

        ratio-input-chan (chan)
        ratio-input (build-ratio-input ratio-input-chan)

        input-state-chan* (chan 1)
        input-state-chan  (chan)

        go-button-chan (chan)
        go-button (build-go-button go-chan)]

    ;; append all this
    
    (state-accumulator initial {:shape shape-input-chan
                                :ratio ratio-input-chan}
                       input-state-chan)
    (repeater input-state-chan* input-state-chan)
    
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

(defn build-go-button [out-chan]
  (let [frag (.createDocumentFragment js/document)
        button (make-button nil "Go!")]
    (.addEventListener button "click"
                       (fn [event]
                         (go (>! out-chan event))))
    button))

(defn render-game
  "A simple renderer"
  [render-surface options game-chan]
  (let [inner-surface (.createDocumentFragment js/document);; (make-g nil nil)
        ]
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

(defn incremental-render-game [render-surface options game-chan]
  (let [{:keys [f interval max initial-n]} options
        initial-n (or initial-n 1)]
    (go
     (loop [n initial-n]
       (when (< n max)
         (render-game render-surface {:n n} game-chan)
         (when interval (<! (timeout interval)))
         (recur (f n)))))))

(defn differential-incremental-render-game [render-surface options game-chan]
  (incremental-render-game
   render-surface
   (merge options {:f (fn [n] (+ n ((options :dn) n)))})
   game-chan))

(defn exponential-incremental-render-game [render-surface options game-chan]
  (differential-incremental-render-game
   render-surface
   (merge options {:dn (fn [n] (* (options :k) n))})
   game-chan))


(defn do-it []
  (let [button-chan (chan)
        go-button (build-go-button button-chan)]
    (dom/append! (css/sel "body") go-button)

    (go (while true
          (<! button-chan)

          (let [svg (make-svg)
                g (make-g {:transform "translate(250,250) scale(3, -3)"})
                game-chan (chan)]
            (dom/append! (css/sel "body") svg)
            (dom/append! svg g)

            (chaos-game initial-state game-chan)
            (exponential-incremental-render-game g {:k 1 :interval 1000 :max 10000} game-chan))))))



