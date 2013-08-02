(ns chaos.input
  (:require [cljs.core.async :as async :refer [>! <! chan timeout dropping-buffer]]
            [chaos.util :refer [map-chan]]
            [chaos.scratch :refer [dedupe float* map-when-chan retrieve-current-value
                                   repeater throttle state-accumulator]]
            [clojure.string :as str])  
  (:require-macros [cljs.core.async.macros :as m :refer [go]]))

(defn listen-for-event [el evt out-chan]
  (.addEventListener el evt
                     (fn [event]
                       (go (>! out-chan event)))))

(defn listen-for-change-events
  "Queues into chan events that (may) have changed the input element"
  [el chan]
  (doseq [type ["propertychange" "keyup" "input" "paste"]]
    (listen-for-event el type chan)))


(defn element-value [el]
  (.-value el))

(defn parse-shape-description [description]  
  (map (fn [line]
         (let [[x-str y-str & garbage] (str/split line \space)
               x (float* x-str)
               y (float* y-str)]
           (if (and x y) [x y]
               nil)))
       (str/split description \newline)))

(defn build-shape-input [opts shape-el out-chan]
  (let [shape-events (chan)
        shape-events-throttled (chan)
        shape-description-text* (chan)
        shape-description-text (chan)]
    
    (listen-for-change-events shape-el shape-events)
    (throttle 200 shape-events  shape-events-throttled)
    (map-chan (fn [e]
                (element-value (.-target e))) shape-events-throttled shape-description-text*)
    (dedupe shape-description-text* shape-description-text)
    (map-when-chan parse-shape-description shape-description-text out-chan)))

(defn build-number-input [opts input-el out-chan]
  (let [el-evts (chan)
        evts-throttled (chan)
        evt-vals (chan) 
        evt-deduped-vals (chan)
        floats out-chan]
    (listen-for-change-events input-el el-evts)
    (throttle (opts :throttle-ms) el-evts evts-throttled)
    (map-chan retrieve-current-value evts-throttled evt-vals)
    (dedupe evt-vals evt-deduped-vals)
    (map-when-chan float* evt-deduped-vals floats)))

(defn bootstrap-input-system [opts
                              initial-vals
                              input-els
                              out-chan]
  (let [shape-input (chan)
        ratio-input (chan)
        iterations-input (chan)
        input-state (chan)
        button-clicked (chan)
        current-state (atom initial-vals)]
    
    (build-shape-input opts (input-els :shape) shape-input)
    (build-number-input opts (input-els :ratio) ratio-input)
    (build-number-input opts (input-els :iterations) iterations-input)

    (state-accumulator initial-vals
                       {:shape shape-input
                        :ratio ratio-input
                        :iterations iterations-input}
                       input-state)
    
    (listen-for-event (input-els :button) "click" button-clicked)

    (go
     (while true
       (let [state (<! input-state)]
         (swap! current-state (constantly state)))))
    
    (go
     (while true
       (<! button-clicked)
       (>! out-chan @current-state)))))

;; chaos.input> (def shape-el (.item (.getElementsByName js/document "chaos-shape") 0))
;; #<[object HTMLTextAreaElement]>
;; nil
;; chaos.input> (def ratio-el (.item (.getElementsByName js/document "ratio") 0))
;; nil
;; chaos.input> (def ratio-el (.item (.getElementsByName js/document "chaos-ratio") 0))
;; #<[object HTMLInputElement]>
;; nil
;; chaos.input> (def iterations-el (.item (.getElementsByName js/document "chaos-iterations") 0))a
;; #<[object HTMLInputElement]>
;; nil
;; WARNING: Use of undeclared Var chaos.input/a
;; nil
;; chaos.input> (def button-el (.item (.getElementsByTagName js/document "button") 0))
;; #<[object HTMLButtonElement]>
;; nil
;; chaos.input> (def out-chan (chan))
;; #<[object Object]>
;; nil
;; chaos.input> (bootstrap-input-system {:throttle-ms 200 :throttle 200}
;;                                      {:ratio 0 :iterations 0 :shape []}
;;                                      {:ratio ratio-el :iterations iterations-el
;;                                       :shape shape-el :button button-el}
;;                                      out-chan)



(def shape-el (.item (.getElementsByName js/document "chaos-shape") 0))
(def ratio-el (.item (.getElementsByName js/document "chaos-ratio") 0))
(def iterations-el (.item (.getElementsByName js/document "chaos-iterations") 0))
(def button-el (.item (.getElementsByTagName js/document "button") 0))
(def out-chan (chan))
(bootstrap-input-system {:throttle-ms 200 :throttle 200}
                                     {:ratio 0 :iterations 0 :shape []}
                                     {:ratio ratio-el :iterations iterations-el
                                      :shape shape-el :button button-el}
                                     out-chan)

(def ctrl (chan))
(go (loop []
      (let [[val ch] (alts! [ctrl out-chan])]
           (condp = ch
             ctrl nil
             out-chan (do (.log js/console (str val))
                          (recur))))))