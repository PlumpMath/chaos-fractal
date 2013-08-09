(ns chaos.html)

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
       (when inner (set! (.-innerHTML el) inner))
       el)))

(defn make-el-ns [ns el-name attrs]
  (let [el (.createElementNS js/document ns el-name)]
    (doseq [[k v] attrs]
      (.setAttributeNS el nil (name k) v))
    el))


(def make-button (partial make-el "button"))
(def make-option (partial make-el "option"))

(def make-svg (comp (partial make-el-ns svg-ns "svg")
                    (partial merge {:version "1.1"})))
(def make-g (partial make-el-ns svg-ns "g"))
(def make-circle (partial make-el-ns svg-ns "circle"))
(def make-rect (partial make-el-ns svg-ns "rect"))
