(ns chaos.boot
  (:require [chaos.fractal :as fractal :refer [chaos-game]]
            [chaos.html :as html]
            [chaos.input :as input]
            [chaos.util :refer [map-chan ch->seq]]            
            [cljs.core.async :as async :refer [>! <! chan timeout]]            
            [domina :as dom]
            [domina.css :as css]
            [clojure.string :as str])
  (:require-macros [cljs.core.async.macros :as m :refer [go alts! alt!]]))

(defn render-game
  "A simple renderer"
  [render-surface options game-chan]
  (let [inner-surface (.createDocumentFragment js/document)]
    (go   
     (loop [i (options :n)]
       (when (> i 0)
         (let [p (<! game-chan)]
           (dom/append! inner-surface (html/make-circle
                                       {:cx (first p)
                                        :cy (second p)
                                        :r 1})))
         (recur (dec i))))

     (dom/append! render-surface inner-surface))))

(defn exponential-incremental-render-game [render-surface options game-chan]
  (let [{:keys [interval max initial-n]} options
        initial-n (or initial-n 1)]
    (go
     (loop [n 1]
       (when (< n max)
         (render-game render-surface {:n n} game-chan)
         (when interval (<! (timeout interval)))
         (recur (* 2 n)))))))

(defn first-by-class [container class]
  (.item (.getElementsByClassName container class) 0))

(defn generate-shape [r n]
  (let [d-theta (/ (* 2 Math/PI) n)
        theta0 (/ Math/PI 2)]
    (for [theta (map (fn [x] (+ theta0 (* x d-theta )))
                     (range 0 n))]
      [(* r (Math/cos theta)) (* r (Math/sin theta))])))

(def shapes
  (map vector
       ["triangle" "square" "pentagon" "hexagon" "septagon" "octogon"]
       (map (fn [x] (generate-shape 100 (+ 3 n))) (range);; errr.... domain
            )))

(defn ^:export attach-to [container]
  (let [shape-el (first-by-class container "chaos-shape")
        shape-select-el (first-by-class container "chaos-shape-select")
        ratio-el (first-by-class container "chaos-ratio")
        iterations-el (first-by-class container "chaos-iterations")
        button-el (first-by-class container "chaos-button")
        render-el (first-by-class container "chaos-render")
        out-chan (chan)]
    
    (input/bootstrap-input-system {:throttle-ms 200
                                   :shapes shapes}
                                  {:ratio 0.5 :iterations 10e3 :shape [[0 250]
                                                                       [-250 -250]
                                                                       [250 -250]]}
                                  {:ratio ratio-el :iterations iterations-el
                                   :shape shape-el :shapes-select shape-select-el
                                   :button button-el}
                                  out-chan)
    
    (go (while true
          (let [definition (<! out-chan)
                svg (html/make-svg {:width 500 :height 500})
                rect-bg (html/make-rect {:width "100%" :height "100%" :fill "white"})
                g (html/make-g {:transform "translate(250, 250), scale(1, -1)"})
                game-chan (chan)]
            (dom/prepend! render-el svg)
            (dom/append! svg rect-bg)
            (dom/append! svg g)
            (fractal/chaos-game
             (merge {:initial-position [(- (* 200 (.random js/Math)) 100) 
                                        (- (* 200 (.random js/Math)) 100)]
                     :next-position fractal/next-position}
                    (select-keys definition [:shape :ratio]))
             game-chan)
            (exponential-incremental-render-game g {:interval 500 :max (:iterations definition)} game-chan))))))


(defn ^:export attach-example [container]
  (let [button-el (first-by-class container "chaos-button")
        render-target (css/sel container "g")
        button-chan (chan)
        game-chan (chan)]
    (input/listen-for-event button-el "click" button-chan)
    (go
     (<! button-chan)
     (let [game-chan (chan)]
       (fractal/chaos-game
        {:initial-position [0 0]
         :next-position fractal/next-position
         :ratio 0.5
         :iterations 10e3
         :shape [[0 250]
                 [-250 -250]
                 [250 -250]]}
        game-chan)
       (exponential-incremental-render-game
        render-target
        {:interval 500 :max 10e3}
        game-chan)))))
