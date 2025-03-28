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
  {:tasks [
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
   :selectedTask nil
   })

(def state-atom (r/atom initial-state))

(defn state-tasks->canvas-nodes [state-node]
  {:id (:id state-node)
   :position {:x 0 :y 0}
   :sourcePosition "right"
   :targetPosition "left"
   :data {:label (:label state-node)
          :description (:description state-node)}})

(defn state-tasks-to-canvas-edges [tasks]
  ; for each node, for each of its dependencies, create an edge from the dependency to the node
    (mapcat (fn [task]
                (map (fn [dependency]
                     {:id (str "edge-" (:id task) "-" dependency)
                        :source dependency
                        :target (:id task)})
                     (:dependencies task)))
            tasks))

(defn get-row-by-id [id nodes]
  (->> nodes
       (filter #(= id (:id %)))
       (first)))

(defn get-name-for-id [id nodes]
  (->> nodes
       (filter #(= id (:id %)))
       (map :label)
       (first)))

;; TODO: move to ui/layout.cljs
(defn get-layouted-nodes [nodes edges]
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
    (map (fn [node]
           (let [position (.node g (:id node))
                 width (get-in node [:measured :width] 150)
                 height (get-in node [:measured :height] 0)
                 x (- (.-x position) (/ width 2))
                 y (- (.-y position) (/ height 2))]
             (assoc node :position {:x x :y y})))
         nodes)))

(def mark-selected-node
  (memoize
   (fn [nodes selected-task]
     (if selected-task
       (map (fn [node]
              (if (= (:id node) selected-task)
                (merge node {:className "selected-node"
                             :style {:border "2px solid #FF0000"}})
                node))
            nodes)
       nodes))))


(defn on-node-hover [_ node]
  (cond (not (= (.-id node) (:selectedTask @state-atom)))
    (swap! state-atom update :selectedTask (fn [_] (.-id node))))
  )

(defn on-node-leave [_ node]
  (swap! state-atom update :selectedTask (fn [_] nil)))

(defn handle-row-update [row]
  (let [clj-row (js->clj row :keywordize-keys true)]
    ;; (print @state-atom)
    (swap! state-atom update-in [:tasks] (fn [tasks] (map #(if (= (:id %) (:id clj-row)) clj-row %) tasks)))
    ; return updated row
    (clj->js (get-row-by-id (:id clj-row) (:tasks @state-atom)))))


(def columns [{:field "id" :headerName "ID" :width 100}
              {:field "label" :headerName "Label" :editable true :width 200}
              {:field "description" :headerName "Description" :editable true :width 300}
              {:field "dependencies" :headerName "Dependencies" :editable false :width 300
               :valueGetter (fn [_, row] (str/join ", " (map #(get-name-for-id % (:tasks @state-atom)) (.. row -dependencies))))}])



(defui app []
  (let [
        state (urf/use-reaction state-atom)
        canvas-edges (state-tasks-to-canvas-edges (:tasks state))
        canvas-nodes (as-> (:tasks state) val
                                  (map state-tasks->canvas-nodes val)
                                  (get-layouted-nodes val canvas-edges)
                                  (mark-selected-node val (:selectedTask state)))
        ]
    ($ :div {:style {:height "60vh" :width "100%"}}
       ($ :h1 nil "PERT Canvas")
       ($ :p nil "Active task is " (:selectedTask state))
       ($ :div {:style {:height "50vh"}}
          ($ ReactFlow (clj->js
                        ; TODO: clean this up
                        {:nodes canvas-nodes
                         :edges canvas-edges
                         :onNodeMouseEnter on-node-hover
                         :onNodeMouseLeave on-node-leave
                         ;; :onNodesChange (fn [nodes] (print nodes))
                         :fitView true
                         })
             ($ Background nil)
             ($ Controls nil)))
       ($ :div {:style {:display "block"
                        :height "50vh"
                        :width "100%"}}
          ($ DataGrid {:rows (clj->js (:tasks state))
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
