(ns chaos-fractal.core
  (:require [domina :as dom]))


;; it's going to take too long to get this working
(comment (def system
           '#{{:name random-vertex
               :source :world
               :in nil
               :out [[v Point]]
               :repr :block}

              {:name 'join
               :in [[v Point]
                    [p Point]]
               :out [x (Point Point)]}})



         (def block {})


         (defn draw-block [block]
           '[:rect {:x ?x
                    :y ?y
                    :width ?width
                    :height ?height}]))

;; so let's do this instead

(def default-state
  {:definition
   {:shape []
    :ratio
    :start-position}
   
   :operational-parameters
   {:iterations 10000
    :animate {:on false
              :step-size 100}}})

(declare create-div render-definition render-operational-parameters)
(defn render-state [at s]
  ;; cleanup any existing state
  (dom/destroy-children! at) ;; that's pretty harsh!

  (let [{:keys [definition operational-parameters]}]
    
    ;; create definition box
    (let [target (create-div at :definition)]
      (render-definition target definition))

    (let [target (create-div at :operational-parameters)]
      (render-operational-parameters target operational-parameters))))

(defn create-div [at id])

(defn render-definition [at {:as definition :keys [shape ratio start-position]}])