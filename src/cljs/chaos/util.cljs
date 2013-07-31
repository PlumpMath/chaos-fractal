(ns chaos.util
    (:require [cljs.core.async :as async :refer [>! <! chan timeout]])
    (:require-macros [cljs.core.async.macros :as m :refer [go]]))

(defn randoms [n out-chan]  
  (go (while true
        (>! out-chan (rand-int n)))))

(defn map-chan [f in-chan out-chan]  
  (go (while true (>! out-chan (f (<! in-chan))))))

(defn ch->seq [ch]
  (lazy-seq
   (cons (<!! ch)
         (ch->seq ch))))
