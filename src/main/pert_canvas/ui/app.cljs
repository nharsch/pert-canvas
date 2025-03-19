(ns pert-canvas.ui.app
  (:require [uix.core :as uix :refer [defui $]]
            [uix.dom]
            [uix.re-frame :as urf]
            [reagent.core :as r]
            [clojure.string :as str]
            ["@xyflow/react" :refer [ReactFlow Background Controls]]
            ["@dagrejs/dagre" :as Dagre]
            ["@mui/x-data-grid" :refer [DataGrid]]
            ))

(def initial-state
  {:nodes [
           {:id "1"
            :label "Node 1"
            :description "Node 1 description"}
           {:id "2"
            :label "Node 2"
            :description "Node 2 description"
            :dependencies ["1"]}
           {:id "3"
            :label "Node 3"
            :description "Node 3 description"
            :dependencies ["2"]}
           {:id "4"
            :label "Node 4"
            :description "Node 4 description"
            :dependencies ["2"]}
           {:id "5"
            :label "Node 5"
            :description "Node 5 description"
            :dependencies ["3"]}
           {:id "6"
            :label "Node 6"
            :description "Node 6 description"
            :dependencies ["4" "5"]}
           ]
   })

(defn state-node->canvas-node [state-node]
  {:id (:id state-node)
   :position {:x 0 :y 0}
   :sourcePosition "right"
   :targetPosition "left"
   :data {:label (:label state-node)
          :description (:description state-node)}})

(defn state-nodes->canvas-edges [nodes]
  ; for each node, for each of its dependencies, create an edge from the dependency to the node
    (mapcat (fn [node]
                (map (fn [dependency]
                     {:id (str "edge-" (:id node) "-" dependency)
                        :source dependency
                        :target (:id node)})
                     (:dependencies node)))
            nodes))


(get-in {:id 0} [:measured :width] 0)

;; TODO: move to ui/layout.cljs
(defn get-layouted-elements [nodes edges]
  (let [g (-> (new (.. Dagre -graphlib -Graph))
              (.setDefaultEdgeLabel (fn [] #js {})))]
    (.setGraph g #js {:rankdir "LR"})

    (doseq [edge edges]
      (.setEdge g (:source edge) (:target edge)))

    (doseq [node nodes]
      (.setNode g (:id node)
                (clj->js (merge node
                                {:width (get-in node [:measured :width] 150)
                                 :height (get-in node [:measured :height] 0)}))))

    (.layout Dagre g)

    {:nodes (map (fn [node]
                   (let [position (.node g (:id node))
                         width (get-in node [:measured :width] 150)
                         height (get-in node [:measured :height] 0)
                         x (- (.-x position) (/ width 2))
                         y (- (.-y position) (/ height 2))]
                     (assoc node :position {:x x :y y})))
                 nodes)
     :edges edges}))

(.-dependencies (clj->js (nth (:nodes initial-state) 4)))

(defn get-row-by-id [id nodes]
  (->> nodes
       (filter #(= id (:id %)))
       (first)))

(defn get-name-for-id [id nodes]
  (->> nodes
       (filter #(= id (:id %)))
       (map :label)
       (first)))

(get-name-for-id "2" (:nodes initial-state))


                                        ; TODO: is there a simpler way to get reactive state? https://github.com/pitch-io/uix/blob/master/docs/interop-with-reagent.md#syncing-with-ratoms-and-re-frame
(def state-atom (r/atom initial-state))

(defn handle-row-update [row]
  (let [clj-row (js->clj row :keywordize-keys true)]
    (print @state-atom)
    (swap! state-atom update-in [:nodes] (fn [nodes] (map #(if (= (:id %) (:id clj-row)) clj-row %) nodes)))
    ; return updated row
    (clj->js (get-row-by-id (:id clj-row) (:nodes @state-atom)))))

(def columns [{:field "id" :headerName "ID" :width 100}
              {:field "label" :headerName "Label" :editable true :width 200}
              {:field "description" :headerName "Description" :editable true :width 300}
              {:field "dependencies" :headerName "Dependencies" :editable false :width 300
               :valueGetter (fn [_, row] (str/join ", " (map #(get-name-for-id % (:nodes @state-atom)) (.. row -dependencies))))}])

(defui app []
  (let [state (urf/use-reaction state-atom)]
    ($ :div {:style {:height "60vh" :width "100%"}}
       ($ :h1 nil "PERT Canvas")
       ($ :div {:style {:height "50vh"}}
          ($ ReactFlow (clj->js (get-layouted-elements (map state-node->canvas-node (:nodes state))
                                                       (state-nodes->canvas-edges (:nodes state))))
             ($ Background nil)
             ($ Controls nil)))
       ($ :div {:style {:display "block"
                        :height "50vh"
                        :width "100%"}}
          ($ DataGrid {:rows (clj->js (:nodes state))
                       :columns (clj->js columns)
                       ;; :processRowUpdate (fn [row] (swap! state-atom update-in [:nodes] (fn [nodes] (map #(if (= (:id %) (:id row)) row %) nodes))))
                       :processRowUpdate handle-row-update
                       :onProcessRowUpdateError (fn [error] (print error))
                       }
             )))))


(defonce root
  (uix.dom/create-root (js/document.getElementById "root")))


(defn ^:dev/after-load init []
  (print "Hello, world!")
  (uix.dom/render-root ($ app) root))
