(ns chaos.scratch
  (:require [chaos.util :refer [map-chan]]
            [cljs.core.async :as async :refer [>! >!! <! <!! chan timeout]]            
            [domina :as dom]
            [domina.css :as css])
  (:require-macros [cljs.core.async.macros :as m :refer [go alts! alt!]]))


(defn make-div []
  (first (dom/nodes (dom/html-to-dom "<div/>"))))

(defn number-input
  "Just a plain old text input for now"
  [name init]
  (first
   (dom/nodes
    (dom/html-to-dom
     (str "<input type=\"text\" name=\"" name "\" value=\""  init "\" />")))))

(defn add-new-container []
  (let [body (css/sel "body")
        container (make-div)]
    (dom/append! body container)
    container))


(defn listen-for-change-events
  "Queues into chan events that (may) have changed the input element"
  [input chan]
  (doseq [type ["propertychange" "keyup" "input" "paste"]]
    (.addEventListener input type
                       (fn [event]
                         (go (>! chan event))))))


(defn map-when-chan ;; maybe-map-chan?
  [f in-chan out-chan]
  (go (while true
        (when-let [v (f (<! in-chan))]
          (>! out-chan v)))))

(defn retrieve-current-value
  "Assumes the event is about an input that has a useful value attribute"
  [event]
  (-> event
      (aget "target")
      (aget "value")))

(defn dedupe [in-chan out-chan]
  (go
   (loop [prev-val ::init]
     (let [cur-val (<! in-chan)]
       (if (not= cur-val prev-val)
         (>! out-chan cur-val))
       (recur cur-val)))))

(defn bifurcate [f in-chan true-chan false-chan]
  (go
   (while true
     (let [val (<! in-chan)]
       (if (f val)
         (>! true-chan val)
         (>! false-chan val))))))

(defn delay-val [ms val in-chan]
  (go
   (<! (timeout ms))
   (>! in-chan val)))


(defn sink [f chan]
  (go (while true (f (<! chan)))))

;; how to rewrite any process to include this behavior (pause/play)?
(defn controlled-sink [f chan toggle]
  (go
   (while true
     (alt!
      toggle ([_] (<! toggle))  ;; pause/play...
      chan ([v] (f v))))))

(defn throttle
  "Receives a value from in-chan and forwards it to out-chan after
  delay-ms only if no other value have arrived at in-chan. 

  If a value does arrive at in-chan while waiting to propagate the
  previous value, this new value will be propagated according the
  same rule.

  This explanation isn't very clear.

  If out-chan is sinked to println i.e. 
    (go (while true (println (<! out-chan))))

  Then:
    (throttle 500 in-chan out-chan)
  
  user>  (>!! in-chan 1)(>!! in-chan 2)(>!! in-chan 3)(>!! in-chan 4)(>!! in-chan 5)(>!! in-chan 6)
  
  Will print only 6."  
  [delay-ms in-chan out-chan]
  (go
   (loop [delayed (chan)]
     (recur
      (alt!
       in-chan ([val]
                  (let [delay-chan (chan)]
                    (delay-val delay-ms val delay-chan)
                    delay-chan))
       delayed ([val]
                  (>! out-chan val)
                  (chan)))))))


;; now filter floats

(defn filter-chan [f in-chan out-chan]
  (go
   (while true
     (let [v (<! in-chan)] (when (f v) (>! out-chan v))))))

(defn float* [x]
  (let [xf (js/parseFloat x)]
    (if-not (js/isNaN xf)
      xf)))

(defn build-number-input [opts input-el out-chan]
  (let [el-evts (chan)
        evts-throttled (chan) ;; does it matter where you put this?
        evt-vals (chan) 
        evt-deduped-vals (chan)
        floats out-chan]
    (listen-for-change-events input-el el-evts)
    (throttle (opts :throttle-ms) el-evts evts-throttled)
    (map-chan retrieve-current-value evts-throttled evt-vals)
    (dedupe evt-vals evt-deduped-vals)
    (map-when-chan float* evt-deduped-vals floats)))


(comment
  (defn build-number-input [opts input-el out-chan]
    (run
     (unify-chan [?floats out-chan]
                 (listen-for-change-events ?input-el ?el-evts)    
                 (map-chan retrieve-current-value ?el-evts ?evt-vals)
                 (throttle (opts :throttle-ms) ?evt-vals ?evts-throttled)
                 (dedupe evts-throttled ?evt-deduped-vals)
                 (map-when-chan float* ?evt-deduped-vals ?floats))))
  
  (macroexpand-1
   (run
    (unify-chan [?floats out-chan]
                (debug-machine ;; here
                 (listen-for-change-events input-el ?el-evts)    
                 (map-chan retrieve-current-value ?el-evts ?evt-vals)
                 (throttle (opts :throttle-ms) ?evt-vals ?evts-throttled)
                 (dedupe evts-throttled ?evt-deduped-vals)
                 (map-when-chan float* ?evt-deduped-vals ?floats)))))
  ;; result
  '(run
    (unify-chan [?floats out-chan]               
                (listen-for-change-events input-el ?el-evts-1)    
                (map-chan retrieve-current-value ?el-evts-2 ?evt-vals-1)
                (throttle (opts :throttle-ms) ?evt-vals-2 ?evts-throttled-1)
                (dedupe ?evts-throttled-2 ?evt-deduped-vals-1)
                (map-when-chan float* ?evt-deduped-vals-2 ?floats)
                (debugger '{?el-evts-1 ?el-evts-2
                            ?evt-vals-1 ?evt-vals-2
                            ?evts-throttled-1 ?evts-throttled-2
                            ?evt-deduped-vals-1 ?evt-deduped-vals-2}
                          '[(listen-for-change-events input-el ?el-evts-1)    
                            (map-chan retrieve-current-value ?el-evts-2 ?evt-vals-1)
                            (throttle (opts :throttle-ms) ?evt-vals-2 ?evts-throttled-1)
                            (dedupe evts-throttled-2 ?evt-deduped-vals-1)
                            (map-when-chan float* ?evt-deduped-vals-1 ?floats)])))


  (defn debug-build-number-input [opts input-el out-chan]
    (run
     (unify-chan [?floats out-chan]
                 (debug-machine
                  {:post-hook (fn [machine] (custom-transform-machine machine))}
                  (listen-for-change-events input-el ?el-evts)    
                  (map-chan retrieve-current-value ?el-evts ?evt-vals)
                  (throttle (opts :throttle-ms) ?evt-vals ?evts-throttled)
                  (dedupe evts-throttled ?evt-deduped-vals)
                  (map-when-chan float* ?evt-deduped-vals ?floats))))))


(defn state-accumulator [initial-state chan-map out-chan]
  (let [reverse-map (into {} (map (comp (partial apply vector) reverse) chan-map))
        ;; the above call to `vector` during mapping is required
        ;; because `conj` (on which reverse-map relies) requires
        ;; vectors as input - lists are rejected
        chans (keys reverse-map)]
    (go
     (loop [state initial-state]
       (let [[val ch] (alts! chans)
             key (reverse-map ch)
             state* (assoc state key val)]
         (>! out-chan state*)
         (recur state*))))))

(defn do-it []
  (let [container (add-new-container)
        input (number-input "abc")
        out-chan (chan)]
    (dom/append! container input)
    (build-number-input {:throttle-ms 200} input out-chan)
    (sink (fn [v] (.log js/console v)) out-chan)))

(defn pair-chan [initial-state left-chan right-chan out-chan]
  (go
   (loop [pair initial-state]
     (>! out-chan pair)
     (recur (alt!
             left-chan ([left] [left (second pair)])
             right-chan ([right] [(first pair) right]))))))

(defn cons-chan [initial-state left-chan right-chan out-chan]
  (go
   (loop [[car & cdr] initial-state]
     (>! out-chan (cons car cdr))
     (recur (alt!
             left-chan ([car] (cons car cdr))
             right-chan ([cdr] (cons car cdr)))))))

(defn do-it []
  (let [container (add-new-container)
        x-input (number-input "x")
        y-input (number-input "y")
        x-chan (chan)
        y-chan (chan)
        xy-chan (chan)]
    (dom/append! container x-input)
    (dom/append! container y-input)

    (build-number-input {:throttle-ms 200} x-input x-chan)
    (build-number-input {:throttle-ms 200} y-input y-chan)
    (pair-chan [0 0] x-chan y-chan xy-chan)
    
    (sink (fn [v] (.log js/console (pr-str v))) xy-chan)))

;; a better make point input
;; - should take initial state as input
;; - css styling?
;; - ???

(defn point-input [x-el y-el opts init x-chan y-chan xy-chan]
  (build-number-input opts x-el x-chan)
  (build-number-input opts y-el y-chan)
  (pair-chan init x-chan y-chan xy-chan))

(defn easy-point-input [opts init x-chan y-chan xy-chan]
  (let [container (.createDocumentFragment js/document)
        x-input (number-input "x" (first init))
        y-input (number-input "y" (second init))]
    (dom/append! container x-input)
    (dom/append! container y-input)
    (point-input x-input y-input opts init x-chan y-chan xy-chan)    
    container))


(defn do-add [add! vect item ctrl]
  (add! item ctrl)
  (conj vect item))

;; how does the item know how to remove itself?

(defn do-remove [remove vect pred]
  (let [idx (ffirst (filter (comp pred second) (map list range vect)))]
    ()))

(defn vector-manager [init add remove in-chan out-chan]
  (go
   (loop [vect init]     
     (let [[cmd arg] (<! in-chan)]
       (condp = cmd
         :add (let [vect* (do-add add vect arg)]
                (>! out-chan vect*)
                (recur vect*)) 
         :remove (let [vect* (do-remove remove vect arg)]
                   (>! out-chan vect*)
                   (recur vect*)))))))


;; maybe use this in programming a game?  e.g. rendering a scene
;; requires positioning all the mobs... so read all the mob channels.
;; if a mob doesn't act or move you still want its position.  could
;; aggregate mob state in a state accumulator?
(defn repeater [in-chan out-chan]  
  (go   
   (loop [val (<! in-chan)]
     (recur
      (alt!
       in-chan ([val'] (>! out-chan val') val')
       :default (do (>! out-chan val) val))))))


;; it's going to take too long to get this working
;; (comment (def system
;;            '#{{:name random-vertex
;;                :source :world
;;                :in nil
;;                :out [[v Point]]
;;                :repr :block}

;;               {:name 'join
;;                :in [[v Point]
;;                     [p Point]]
;;                :out [x (Point Point)]}})



;;          (def block {})


;;          (defn draw-block [block]
;;            '[:rect {:x ?x
;;                     :y ?y
;;                     :width ?width
;;                     :height ?height}]))

;; ;; so let's do this instead

;; (def default-state
;;   {:definition
;;    {:shape []
;;     :ratio
;;     :start-position}
   
;;    :operational-parameters
;;    {:iterations 10000
;;     :animate {:on false
;;               :step-size 100}}})

;; (declare create-div render-definition render-operational-parameters)
;; (defn render-state [at s]
;;   ;; cleanup any existing state
;;   (dom/destroy-children! at) ;; that's pretty harsh!

;;   (let [{:keys [definition operational-parameters]}]
    
;;     ;; create definition box
;;     (let [target (create-div at :definition)]
;;       (render-definition target definition))

;;     (let [target (create-div at :operational-parameters)]
;;       (render-operational-parameters target operational-parameters))))

;; (defn create-div [at id])

;; (defn render-definition [at {:as definition :keys [shape ratio
;; start-position]}])

