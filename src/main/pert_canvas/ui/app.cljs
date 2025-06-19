(ns pert-canvas.ui.app
  (:require [uix.core :as uix :refer [defui defhook $]]
            [uix.dom]
            [uix.re-frame :as urf]
            [reagent.core :as r]
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
    (mapv int [source-id target-id])))

(defn remove-dep-from-row [dep-id row]
  (let [dependencies (:dependencies row)]
    (assoc row :dependencies (remove #(= % dep-id) dependencies))))


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

(defn parse-dependencies [val]
  (mapv int (str/split val #",\s*")))


;; state management
;; (def app-db (r/atom initial-state))


(rf/reg-event-fx
 :initialize-db
 (println ":initialize-db")
 (fn [_ _]
   {:db initial-state}))


(rf/reg-event-db
 :hover-node
 (fn [db [_ id]]
   (assoc db :app/hovered-task (int id))))

(rf/reg-event-db
 :hover-row
 (fn [db [_ id]]
   (assoc db :app/hovered-task (int id))))


(rf/reg-event-db
 :select-row
 (fn [db [_ selected-id]]
   (println "select-row" selected-id)
   (assoc db :app/selected-task (int selected-id))))



(rf/reg-event-db
 :unselect-row
 (fn [db [_ id]]
   (println "unselect-row" id)
   (assoc db :app/selected-task nil)))

(rf/reg-event-db
 :leave-node
 (fn [db [_ id]]
   (assoc db :app/hovered-task nil)))

(rf/reg-sub
 :app/selected-task
 (fn [db _]
   (:app/selected-task db)))


(rf/reg-sub
 :app/tasks
 (fn [db _]
   (:app/tasks db)))

(comment
  (println (->> @(rf/subscribe [:app/tasks])
                (map :selected)))
  (println (->> @(rf/subscribe [:app/selected-task])
                (map :id)))
  )

(rf/reg-event-db
 :create-connection
 (fn [db [_ source-id target-id]]
   (println "create-connection" source-id target-id)
   (update-in db [:app/tasks]
              (fn [tasks]
                (map #(if (= (:id %) (int target-id))
                        (update % :dependencies conj source-id)
                        %) tasks)))))

(rf/reg-event-db
 :update-row
 (fn [db [_ row]]
   (update-in db [:app/tasks]
              (fn [tasks]
                (map #(if (= (:id %) (:id row)) row %) tasks)))))

(defn handle-row-update [js-row]
  ;; TODO: validate row before dispatching
  (let [row (js->clj js-row :keywordize-keys true)]
    (rf/dispatch [:update-row row])
    (clj->js row)))

(rf/reg-event-db
 :select-edge
 (fn [db [_ edge-id]]
   (println "select-edge" edge-id)
   (assoc db :app/selected-edge edge-id)))

(defn handle-edge-selection [edges]
  (let [edge (first edges)]
    (rf/dispatch [:select-edge (.-id edge)])))


(defn delete-task-from-db
  [db task-id]
  (println "delete-task" task-id)
  (let [new-tasks
        (update db :app/tasks
                (fn [tasks]
                  (remove #(= (:id %) (int task-id)) tasks)))]
    (println "db after delete" (map :id (:app/tasks new-tasks)))
    new-tasks)
  )

(defn delete-edge-from-db
  [db edge-id]
  (println "delete-edge" edge-id)
  (let [[source-id target-id] (edgeid->ids edge-id)]
    (update-in db [:app/tasks]
               (fn [tasks]
                 (map #(if (= (:id %) source-id)
                         (remove-dep-from-row target-id %)
                         %) tasks)))))

(rf/reg-event-db
 :delete-selected
 (fn [db _]
   (println "delete-selected" (:app/selected-task db) (:app/selected-edge db))
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
 :add-task
 (fn [db _]
   (let [new-id (inc (count (:app/tasks db)))
         new-task {:id new-id
                   :label (str "Node " new-id)
                   :description (str "Node " new-id " description")
                   :dependencies []}]
     (-> db
         (update-in [:app/tasks] conj new-task)
         (assoc :app/selected-task new-id)))))



(defn state-tasks->canvas-nodes [state-node]
  {:id (str (:id state-node))
   :position {:x 0 :y 0}
   :sourcePosition "right"
   :targetPosition "left"
   :selected (:selected state-node false)
   :dependencies (map str (:dependencies state-node))
   :data {:label (:label state-node)
          :description (:description state-node)}})

(defn mark-task-selected [task-id tasks]
  (map #(if (= (:id %) (int task-id))
          (assoc % :selected true)
          %)
       tasks))

(def tasks-selected->canvas-nodes
  (memoize
   (fn [tasks selected-task]
     (let [canvas-edges (tasks->canvas-edges tasks)]
       (as-> tasks ts
         (mark-task-selected selected-task ts)
         (map state-tasks->canvas-nodes ts)
         (get-layouted-nodes ts canvas-edges))))))
(comment
  (->> initial-tasks
       (mark-task-selected 1)
       (map state-tasks->canvas-nodes)
       get-layouted-nodes)
  (tasks-selected->canvas-nodes initial-tasks 2)
)

;; app
;; TODO: make this subscription only update on changes to tasks or selected-task
(rf/reg-sub
 :reactflow/config
 :<- [:app/tasks]
 :<- [:app/selected-task]
 (memoize
  (fn [[tasks selected-task] _]
    (let [nodes (tasks-selected->canvas-nodes tasks selected-task)
          edges (tasks->canvas-edges tasks)]
      (clj->js
       {:nodes nodes
        :edges edges
        :onNodeMouseEnter #(rf/dispatch [:hover-node (int (.-id %2))])
        :onNodeMouseLeave #(rf/dispatch [:leave-node (int (.-id %2))])
        :onNodeClick #(rf/dispatch [:select-row (int (.-id %2))])
        :onConnect #(rf/dispatch [:create-connection (.-source %) (.-target %)])
        :onEdgesChange handle-edge-selection
        :onEdgesDelete (fn [event] (println "on delete: " event))
        :fitView true
        }))
    )))

(comment
  (js/console.log (.-nodes @(rf/subscribe [:reactflow/config])))
  )

(rf/reg-sub
 :datagrid/rows
 :<- [:app/tasks]
 (memoize
  (fn [tasks _]
    (println ":datagrid/rows")
    (->> tasks
         clj->js
         ))))


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
  (uix/use-effect
   (fn []
     (let [handle-keydown (fn [event]
                            (when (or (= (.-key event) "Delete")
                                      (= (.-key event) "Backspace"))
                              (rf/dispatch [:delete-selected])
                              ))]
       (.addEventListener js/document "keydown" handle-keydown)
       ;; Cleanup function
       #(.removeEventListener js/document "keydown" handle-keydown)))
   [])

  (let [selected-task (urf/use-subscribe [:app/selected-task])]
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
          ($ :button {:onClick #(rf/dispatch [:add-task])
                      :style {:margin "10px"}}
             "Add Task")
          ;; TODO: consider recomputing DataGrid config as reactive sub
          ($ DataGrid {
                       :rows (urf/use-subscribe [:datagrid/rows])
                       :columns (clj->js columns)
                       :processRowUpdate handle-row-update
                       :rowSelectionModel (clj->js [selected-task])
                       :onRowClick #(rf/dispatch [:select-row (int (.-id %))] )
                       :onCellEditStart #(rf/dispatch [:select-row (int (.-id %))])
                       :onCellEditStop #(rf/dispatch [:unselect-row (int (.-id %))])
                       :onProcessRowUpdateError (fn [error] (print error))
                       })))))

(comment
  @(rf/subscribe [:app/tasks])
  @(rf/subscribe [:app/selected-task])
  )

(defonce root
  (uix.dom/create-root (js/document.getElementById "root")))


(defn ^:dev/after-load init []
  (print "Hello, world!")
  (rf/dispatch-sync [:initialize-db])
  (uix.dom/render-root ($ app) root))
