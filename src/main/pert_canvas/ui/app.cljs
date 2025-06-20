(ns pert-canvas.ui.app
  (:require [uix.core :as uix :refer [defui $]]
            [uix.dom]
            [uix.re-frame :as urf]
            [re-frame.core :as rf]
            [clojure.string :as str]
            ["@xyflow/react" :refer [ReactFlow Background Controls]]
            ["@dagrejs/dagre" :as Dagre]
            ["@mui/x-data-grid" :refer [DataGrid]]
            ))

(def initial-tasks
  [
   {:id 1
    :label "Buy Ingredients"
    :description "Node 1 description"}
   {:id 2
    :label "Mix Ingredients"
    :description "Node 2 description"
    :dependencies [1]}
   {:id 3
    :label "place dough on pan"
    :description "Node 3 description"
    :dependencies [2]}
   {:id 4
    :label "Bake dough"
    :description "Node 4 description"
    :dependencies [2]}
   {:id 5
    :label "preheat oven"
    :description "Node 5 description"
    :dependencies [3]}
   {:id 6
    :label "eat cookies"
    :description "Node 6 description"
    :dependencies [4 5]}
   ])

(def initial-state
  {
   :app/tasks initial-tasks
   :app/selected-task nil
   :app/selected-edge nil
   })

;; Utils
(defn edgeid->ids [edge-id]
  (let [[_ source-id target-id] (str/split edge-id #"-")]
    (map int [source-id target-id])))

(defn remove-dep-from-row [dep-id row]
  (let [dependencies (:dependencies row)]
    (assoc row :dependencies (remove #(= (int %) (int dep-id)) dependencies))))


(def get-layouted-nodes
  "uses Dagre to update positions on nodes"
  (memoize
   (fn [nodes edges]
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
            nodes)))))


(def tasks->canvas-edges
  (memoize
   (fn [tasks]
     (mapcat (fn [task]
               (map (fn [dependency]
                      {:id (str "edge-" (:id task) "-" dependency)
                       :source (str dependency)
                       :target (str (:id task))})
                    (:dependencies task)))
             tasks))))

(defn state-task->canvas-node [state-task]
  {:id (str (:id state-task))
   :position {:x 0 :y 0}
   :sourcePosition "right"
   :targetPosition "left"
   :selected (:selected state-task false)
   :dependencies (map str (:dependencies state-task))
   :data {:label (:label state-task)
          :description (:description state-task)}})

(defn get-row-by-id [id nodes]
  (->> nodes
       (filter #(= id (:id %)))
       (first)))

(defn get-name-for-id [id nodes]
  (->> nodes
       (filter #(= id (:id %)))
       (map :label)
       (first)))

(defn get-row-by-name [name nodes]
  (->> nodes
       (filter #(= name (:label %)))
       (first)))

(defn delete-task-from-db
  [db task-id]
  (println "delete-task" task-id)
  (let [new-tasks
        (update db :app/tasks
                (fn [tasks]
                  (remove #(= (:id %) (int task-id)) tasks)))]
    new-tasks))

(defn delete-edge-from-db
  [db edge-id]
  (println "delete-edge" edge-id)
  (let [[source-id target-id] (edgeid->ids edge-id)]
    (println "source-id" source-id "target-id" target-id)
    (update-in db [:app/tasks]
               (fn [tasks]
                 (map #(if (= (:id %) source-id)
                         (do
                           (remove-dep-from-row target-id %))
                         %) tasks)))))

(defn parse-dependencies [val]
  (mapv int (str/split val #",\s*")))

(def mark-nodes-selected
  (memoize
   (fn [selected-id nodes]
     (map #(if (= (:id %) (int selected-id))
             (assoc % :selected true)
             %)
          nodes))))

;; Re-frame events and subscriptions
(rf/reg-event-fx
 :initialize-db
 (println ":initialize-db")
 (fn [_ _]
   {:db initial-state}))

(rf/reg-event-db
 :ui/hover-node
 (fn [db [_ id]]
   ;; (println "hover-node" id)
   (assoc db :app/hovered-task (int id))))

(rf/reg-event-db
 :ui/leave-node
 (fn [db [_ id]]
   ;; (println "leave-node" id)
   (assoc db :app/hovered-task nil)))

(rf/reg-event-db
 :ui/select-row
 (fn [db [_ id]]
   ;; (println "select-row" id)
   (assoc db :app/selected-task (int id)
             :app/selected-edge nil)))

(rf/reg-event-db
 :ui/select-node
 (fn [db [_ id]]
   ;; (println "select-node" id)
   (assoc db :app/selected-task (int id)
             :app/selected-edge nil)))

(rf/reg-event-db
 :ui/edit-row-start
 (fn [db [_ id]]
   ;; (println "edit-row-start" id)
   (assoc db :app/selected-task (int id)
          :ui/editing-text true)))

(rf/reg-event-db
 :ui/edit-row-end
 (fn [db [_ id]]
   ;; (println "edit-row-end" id)
   (assoc db :ui/editing-text false)))

(rf/reg-event-db
 :ui/unselect-row
 (fn [db [_ id]]
   ;; (println "unselect-row" id)
   (assoc db :app/selected-task nil)))


(rf/reg-event-db
 :ui/create-connection
 (fn [db [_ source-id target-id]]
   ;; (println "create-connection" source-id target-id)
   (update-in db [:app/tasks]
              (fn [tasks]
                (map #(if (= (:id %) (int target-id))
                        (update % :dependencies conj source-id)
                        %) tasks)))))

(rf/reg-event-db
 :ui/update-row
 (fn [db [_ row]]
   ;; (println "update-row" row)
   (update-in db [:app/tasks]
              (fn [tasks]
                (map #(if (= (:id %) (:id row)) row %) tasks)))))

(rf/reg-event-db
 :ui/select-edge
 (fn [db [_ edge-id]]
   ;; (println "select-edge" edge-id)
   (assoc db :app/selected-edge edge-id
          :app/selected-task nil)))

(rf/reg-event-db
 :ui/delete-selected
 (fn [db _]
   ;; (println "delete-selected" (:app/selected-task db) (:app/selected-edge db))
   (let [selected-task (:app/selected-task db)
         selected-edge (:app/selected-edge db)]
     (cond
       selected-task (-> db
                         (delete-task-from-db selected-task)
                         (assoc :app/selected-task nil))
       selected-edge (-> db
                         (delete-edge-from-db selected-edge)
                         (assoc :app/selected-edge nil))
       :else db))))

(rf/reg-event-db
 :ui/add-task
 (fn [db _]
   ;; (println "add-task")
   (let [new-id (inc (count (:app/tasks db)))
         new-task {:id new-id
                   :label (str "Node " new-id)
                   :description (str "Node " new-id " description")
                   :dependencies []}]
     (-> db
         (update-in [:app/tasks] conj new-task)
         (assoc :app/selected-task new-id)))))


(rf/reg-sub
 :app/selected-task
 (fn [db _]
   ;; (println "sub :app/selected-task")
   (:app/selected-task db)))

(rf/reg-sub
 :app/selected-edge
 (fn [db _]
   ;; (println "sub :app/selected-edge")
   (:app/selected-edge db)))


(rf/reg-sub
 :app/tasks
 (fn [db _]
   ;; (println "sub :app/tasks")
   (:app/tasks db)))

(rf/reg-sub
 :ui/editing-text
 (fn [db _]
   ;; (println "sub :ui/editing-text")
   (:ui/editing-text db)))


(rf/reg-sub
 :reactflow/edges
 :<- [:app/tasks]
 (fn [tasks _]
   ;; (println ":reactflow/edges")
   (tasks->canvas-edges tasks)))

(rf/reg-sub
 :reactflow/layouted-nodes
 :<- [:app/tasks]
 :<- [:reactflow/edges]
 (fn [[tasks edges] _]
   ;; (println ":reactflow/layouted-nodes")
   (get-layouted-nodes (map state-task->canvas-node tasks)
                       edges)))

(rf/reg-sub
 :reactflow/nodes-with-selected
  :<- [:reactflow/layouted-nodes]
  :<- [:app/selected-task]
  (fn [[nodes selected-id] _]
    ;; (println ":reactflow/nodes-with-selected")
    (mark-nodes-selected selected-id nodes)))

;; app
;; TODO: make this subscription only update on changes to tasks or selected-task
(rf/reg-sub
 :reactflow/config
 :<- [:reactflow/nodes-with-selected]
 :<- [:reactflow/edges]
 (fn [[nodes edges] _]
   (println "sub :reactflow/config")
   (clj->js
    {:nodes nodes
     :edges edges
     :onNodeMouseEnter #(rf/dispatch [:ui/hover-node (int (.-id %2))])
     :onNodeMouseLeave #(rf/dispatch [:ui/leave-node (int (.-id %2))])
     :onNodeClick #(rf/dispatch [:ui/select-node (int (.-id %2))])
     :onConnect #(rf/dispatch [:ui/create-connection (.-source %) (.-target %)])
     :onEdgesChange (fn [edges] (rf/dispatch [:ui/select-edge (.-id (first edges))]))
     ;; :onEdgesDelete (fn [event] (println "on delete: " event))
     :fitView true
     })
   ))

(rf/reg-sub
 :datagrid/rows
 :<- [:app/tasks]
 (fn [tasks _]
   (println ":datagrid/rows")
   (clj->js tasks)))


(defn handle-row-update [js-row]
  ;; TODO: validate row before dispatching
  (let [row (js->clj js-row :keywordize-keys true)]
    (println "handle-row-update" row)
    (rf/dispatch [:ui/update-row row])
    (clj->js row)))

;; App
(def columns [
              {:field "id" :headerName "ID" :width 100}
              {:field "label" :headerName "Label" :editable true :width 200}
              {:field "description" :headerName "Description" :editable true :width 300}
              {:field "dependencies" :headerName "Dependencies" :editable true :width 300
               :valueGetter (fn [_, row] (str/join ", " (.. row -dependencies)))
               :valueSetter (fn [val, row]
                              (let [dependencies (parse-dependencies val)]
                                (set! (.-dependencies row) dependencies))
                              row)
               }])

(defui app []
  (let [selected-task (urf/use-subscribe [:app/selected-task])
        editing? (urf/use-subscribe [:ui/editing-text])]

    ; delete key handling
    (uix/use-effect
     (fn []
       (let [handle-keydown (fn [event]
                              (println "keydown event" (.-key event))
                              (when (or (= (.-key event) "Delete")
                                        (= (.-key event) "Backspace"))
                                (cond (not editing?)
                                      (do
                                        (rf/dispatch [:ui/delete-selected])
                                        (.preventDefault event)))))]
         (.addEventListener js/document "keydown" handle-keydown)
         ;; Cleanup function
         #(.removeEventListener js/document "keydown" handle-keydown)))
     [editing?])

    ; UI
    ($ :div {:style {:height "60vh" :width "100%"}}
       ($ :h1 nil "PERT Canvas")
       ($ :div {:style {:height "50vh"}}
          ($ ReactFlow (urf/use-subscribe [:reactflow/config])
             ($ Background nil)
             ($ Controls nil)))
       ($ :p nil "Active task is " selected-task)
       ($ :div {:style {:display "block"
                        :height "50vh"
                        :width "100%"}}
          ($ :button {:onClick #(rf/dispatch [:ui/add-task])
                      :style {:margin "10px"}}
             "Add Task")
          ;; TODO: consider recomputing DataGrid config as reactive sub
          ($ DataGrid {
                       :rows (urf/use-subscribe [:datagrid/rows])
                       :columns (clj->js columns)
                       :processRowUpdate handle-row-update
                       :rowSelectionModel (clj->js [selected-task])
                       :onRowClick #(rf/dispatch [:ui/select-row (int (.-id %))] )
                       :onCellEditStart #(rf/dispatch [:ui/edit-row-start (int (.-id %))])
                       :onCellEditStop #(rf/dispatch [:ui/edit-row-end (int (.-id %))])
                       :onProcessRowUpdateError (fn [error] (print error))
                       })))))

(defonce root
  (uix.dom/create-root (js/document.getElementById "root")))

(defn ^:dev/after-load init []
  (rf/dispatch-sync [:initialize-db])
  (uix.dom/render-root ($ app) root))
