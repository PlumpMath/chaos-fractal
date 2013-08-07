(ns chaos.fractal
  (:require [chaos.util :refer [map-chan randoms]]
            [cljs.core.async :as async :refer [>! <! chan timeout]])
  (:require-macros [cljs.core.async.macros :as m :refer [go]]))


(def number-of-vertices count)

(def pick-vertex nth)

(defn random-vertices [shape out-chan]
  (let [c (chan)]
    (randoms (number-of-vertices shape) c)
    (map-chan (partial pick-vertex shape) c out-chan)))

(defn next-position-2d [r a b]
  (let [[ax ay] a
        [bx by] b]
    (map +
         [(* r (- bx ax))
          (* r (- by ay))]
         a)))

(defn next-position [r a b]
  (map +
       (map (partial * r)
            (map - b a))
       a))

;; Interesting behavior yields from the following 'incorrect'
;; next-position definition (at least, given a regular triangle and
;; 1/2 ratio as inputs to the algorithm). (Note that what's happening
;; in this definition is that we are not relating - coupling - the
;; delta to the current position; we return the delta vector as the
;; position vector
(defn next-position-incorrect [r a b]
  (let [[ax ay] a
        [bx by] b]    
    [(* r (- bx ax))
     (* r (- by ay))]))


(defn chaos-game [{:as definition :keys [shape ratio initial-position next-position]} out-chan]
  (let [position-chan (chan)
        vertices-chan (chan)]
    (random-vertices shape vertices-chan)
    (go (loop [position initial-position]
          (let [vertex (<! vertices-chan)
                position' (next-position ratio position vertex)]
            (>! out-chan position')
            (recur position'))))

    (comment      
      (random-vertices shape vertices-chan)
      (reduce-chan (partial next-position ratio) initial-position vertices-chan out-chan)
      
      :where                  
      (fn reduce-chan [f initial in-chan out-chan] ;; aka feeback-loop
        (go (loop [val initial]
              (let [x (<! in-chan)
                    val' (f val x)]
                (>! out-chan val')
                (recur val'))))))))
