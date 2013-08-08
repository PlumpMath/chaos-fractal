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
  (doseq [type ["change" "propertychange" "keyup" "input" "paste"]]
    (listen-for-event el type chan)))

(defn parse-shape-description [description]  
  (map (fn [line]
         (let [[x-str y-str & garbage] (str/split line \space)
               x (float* x-str)
               y (float* y-str)]
           (if (and x y) [x y]
               nil)))
       (str/split description \newline)))

(defn build-shape-input [opts shape-el initial-shape out-chan]
  (let [shape-events (chan)
        shape-events-throttled (chan)
        shape-description-text* (chan)
        shape-description-text (chan)]

    (set! (. shape-el -innerHTML)
          (str/join "\n"
                    (map (partial str/join " ") initial-shape)))
    
    (listen-for-change-events shape-el shape-events)
    (throttle (opts :throttle-ms) shape-events  shape-events-throttled)
    (map-chan (fn [e] (.-value (.-target e))) shape-events-throttled shape-description-text*)
    (dedupe shape-description-text* shape-description-text)
    (map-when-chan parse-shape-description shape-description-text out-chan)))

(defn build-shape-select-input [opts shape-select-el initial-selections out-chan]
  (let [events (chan)
        selected (chan)]

    (listen-for-change-events shape-select-el events)
    (map-chan (fn [e] (.-selectedIndex (.-target shape-select-el))) selected)))

(defn build-number-input [opts input-el initial-val out-chan]
  (let [el-evts (chan)
        evts-throttled (chan)
        evt-vals (chan) 
        evt-deduped-vals (chan)
        floats out-chan]
    (set! (. input-el -value) (str initial-val))
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
        shape-select-input (chan)
        ratio-input (chan)
        iterations-input (chan)
        input-state (chan)
        button-clicked (chan)
        current-state (atom initial-vals)]
    
    (build-shape-input opts (input-els :shape) (initial-vals :shape) shape-input)
    (build-shape-select-input opts (input-els :shape-select) (opts :shapes) shape-select-input)
    (build-number-input opts (input-els :ratio) (initial-vals :ratio) ratio-input)
    (build-number-input opts (input-els :iterations) (initial-vals :iterations) iterations-input)

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
