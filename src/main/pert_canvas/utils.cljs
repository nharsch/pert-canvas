(ns pert-canvas.utils
  (:require
   [clojure.string :as str]
   [clojure.set :refer [difference union]]
   [goog.labs.format.csv :as csv]
   [semantic-csv.core :as sc]
   ["@dagrejs/dagre" :as Dagre]
   ))


(defn remap-headers
  ([row-map] row-map)
  ([row-map column-mapping]
   (let [column-mapping (clojure.set/map-invert column-mapping)]
     (reduce-kv (fn [acc k v]
                  (if-let [new-key (get column-mapping k)]
                    (assoc acc new-key v)
                    (assoc acc k v)))
                {}
                row-map))))

(defn dep-str->int [dep-str]
  (let [found-id (re-find #"[0-9]+" dep-str)]
    (cond found-id
          (int found-id))))

(defn parse-incoming-deps [deps]
  (if (string? deps)
    (let [deps-list (str/split deps #"[;,|]")]
      ;; (println deps-list)
      (set (keep dep-str->int deps-list)))
    deps))

(defn deps-csv->set [val]
  (if (str/blank? val)
    #{}
    (or
     (->> (str/split val #",\s*")
          (map int)
          (set)))))

(defn keywordize-values [m]
  (reduce-kv (fn [acc k v]
               (assoc acc k (if (string? v)
                              (keyword v)
                              v)))
             {}
             m))

(defn edgeid->ids [edge-id]
  (let [[_ source-id target-id] (str/split edge-id #"-")]
    (map int [source-id target-id])))

(defn remove-dep-from-row [dep-id row]
  (let [dependencies (:dependencies row)]
    (assoc row :dependencies (difference dependencies #{dep-id}))))

;; csv
(defn csv->tasks
  ([csv-string] (csv->tasks csv-string {}))
  ([csv-string column-mapping]
   (->> csv-string
        csv/parse
        js->clj
        sc/mappify
        (sc/cast-with js/parseInt {:only [(:id column-mapping)]})
        (sc/cast-with parse-incoming-deps {:only (:dependencies column-mapping)})
        doall
        (mapv #(remap-headers % column-mapping))
        )))

(comment
  (js/parseInt " 45")
  (csv->tasks )
  )

;; layout
(defn state-task->canvas-node [state-task]
  ; TODO: pass into react-flow?
  {:id (str (:id state-task))
   :position {:x 0 :y 0}
   :sourcePosition "right"
   :targetPosition "left"

   :dependencies (map str (:dependencies state-task))
   :data {
          :id (:id state-task)
          :label (str (:id state-task)
                      ": "
                      (:label state-task))
          :description (:description state-task)
          :selected (:selected state-task false)
          }})


(defn get-nodes-dims [node nodes-dims]
    (let [id (:id node)]
        (if-let [dims (get nodes-dims id)]
        dims
        {:width 150 :height 40})))

(def get-layouted-nodes
  "uses Dagre to update positions on nodes"
  ;; TODO: spec nodes as CanvasNode
  (memoize
   (fn [nodes edges nodes-dims]
     (let [g (-> (new (.. Dagre -graphlib -Graph))
                 (.setDefaultEdgeLabel (fn [] #js {})))]
       (.setGraph g #js {:rankdir "LR"})
       ;; set edges
       (doseq [edge edges]
         (.setEdge g (:source edge) (:target edge)))
       ;; set position on dagre
       (doseq [node nodes]
         (.setNode g (:id node)
                   (clj->js (merge node
                                   (get-nodes-dims node nodes-dims)))))
       (.layout Dagre g)
       ;; put positions back into nodes
       (map (fn [node]
              (let [position (.node g (:id node))
                    width (:width (get-nodes-dims node nodes-dims))
                    height (:height (get-nodes-dims node nodes-dims))
                    x (- (.-x position) (/ width 2))
                    y (- (.-y position) (/ height 2))]
                (assoc node :position {:x x :y y})))
            nodes)))))
