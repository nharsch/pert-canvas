(ns pert-canvas.ui.app
  (:require [clojure.string :as str]
            [clojure.set :refer [difference union]]
            [uix.core :as uix :refer [defui $]]
            [uix.dom]
            [uix.re-frame :as urf]
            [re-frame.core :as rf]
            [day8.re-frame.undo :as undo :refer [undoable]]
            [malli.core :as m]
            [malli.error :as me]
            ;; JS
            ["@xyflow/react" :refer [ReactFlow Background Controls]]
            ["@dagrejs/dagre" :as Dagre]
            ["@mui/x-data-grid" :refer [DataGrid]]
            ;; local
            [pert-canvas.ui.components.csv-drop-zone :refer [drop-zone]]
            [pert-canvas.ui.components.csv-import-modal :refer [csv-import-modal]]
            ))

(def state-task
  [:map
   [:id :int]
   [:label :string]
   [:dependencies [:set :int]]])

(def initial-tasks
  [
   {:id 1
    :label "Buy Ingredients"
    :description "Buy Ingredients"}
   {:id 2
    :label "Mix Ingredients"
    :description "Mix Ingredients"
    :dependencies #{1}}
   {:id 3
    :label "place dough on pan"
    :description ""
    :dependencies #{2}}
   {:id 4
    :label "Bake dough"
    :description ""
    :dependencies #{3 5}}
   {:id 5
    :label "Preheat Oven"
    :description ""
    :dependencies #{}}
   {:id 6
    :label "Eat Cookies"
    :description ""
    :dependencies #{4}}
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
    (assoc row :dependencies (difference dependencies #{dep-id}))))


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
  (if (str/blank? val)
    #{}
    (->> (str/split val #",\s*")
         (map int)
         (set))))

(def mark-nodes-selected
  (memoize
   (fn [selected-id nodes]
     (map #(if (= (:id %) (int selected-id))
             (assoc % :selected true)
             %)
          nodes))))

;; TODO: migrate to events ns
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
 :ui/select-edge
 (fn [db [_ edge-id]]
   ;; (println "select-edge" edge-id)
   (assoc db :app/selected-edge edge-id
          :app/selected-task nil)))

;; undoable events
(rf/reg-event-db
 :ui/create-connection
 (undoable "create connection")
 (fn [db [_ source-id target-id]]
   ;; (println "create-connection" source-id target-id)
   (update-in db [:app/tasks]
              (fn [tasks]
                (map #(if (= (:id %) (int target-id))
                        (update % :dependencies union #{(int source-id)})
                        %) tasks)))))

(rf/reg-event-db
 :ui/update-row
 (undoable "update task row")
 (fn [db [_ row]]
   ;; (println "update-row" row)
   (if
       (m/validate state-task row)
       (update-in db [:app/tasks]
                  (fn [tasks]
                    (map #(if (= (:id %) (:id row)) row %) tasks)))
       (println (:errors (m/explain state-task row))))))



(comment
  (me/humanize (m/explain
                state-task
                      {:id 0
                       :label "Initial Task"
                       :description "This is an initial task"
                       :dependencies #{}}))
  )

(rf/reg-event-db
 :ui/delete-selected
 (undoable "delete")
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
 (undoable "add task")
 (fn [db _]
   ;; (println "add-task")
   (let [new-id (inc (count (:app/tasks db)))
         new-task {:id new-id
                   :label (str "Node " new-id)
                   :description (str "Node " new-id " description")
                   :dependencies #{}}]
     (-> db
         (update-in [:app/tasks] conj new-task)
         (assoc :app/selected-task new-id)))))

(rf/reg-event-db
 :csv/set-drag-over
 (fn [db [_ drag-over?]]
   (println "set-drag-over" drag-over?)
   (assoc db :csv/drag-over drag-over?)))

(rf/reg-event-fx
 :csv/file-dropped
 (fn [{:keys [db]} [_ file]]
   (println "csv/file-dropped" (.-name file))
   (let [reader (js/FileReader.)]
     (set! (.-onload reader)
           (fn [e]
             (let [csv-content (-> e .-target .-result)]
               (rf/dispatch [:csv/parse-and-show-modal csv-content (.-name file)]))))
     (.readAsText reader file)
     {:db (assoc db :csv/drag-over false)})))

(rf/reg-event-db
 :csv/parse-and-show-modal
 (fn [db [_ csv-content filename]]
   (let [lines (str/split-lines csv-content)
         headers (when (seq lines)
                   (str/split (first lines) #","))
         sample-rows (take 3 (rest lines))]
     (-> db
         (assoc :csv/modal-open true)
         (assoc :csv/filename filename)
         (assoc :csv/headers headers)
         (assoc :csv/sample-rows sample-rows)
         (assoc :csv/raw-content csv-content)
         (assoc :csv/column-mapping {:id nil :label nil :dependencies nil})))))

(rf/reg-event-db
 :csv/set-column-mapping
 (fn [db [_ field column]]
   (assoc-in db [:csv/column-mapping field] column)))

(rf/reg-event-db
 :csv/close-modal
 (fn [db _]
   (-> db
       (dissoc :csv/modal-open)
       (dissoc :csv/filename)
       (dissoc :csv/headers)
       (dissoc :csv/sample-rows)
       (dissoc :csv/raw-content)
       (dissoc :csv/column-mapping))))


;; TODO: use a CSV parser that returns maps for rows
(rf/reg-event-fx
 :csv/import-tasks
 (fn [{:keys [db]} _]
   (println "csv/import-tasks")
   (let [csv-content (:csv/raw-content db)
         column-mapping (:csv/column-mapping db)
         headers (:csv/headers db)]
     (when (and csv-content column-mapping headers)
       (let [header-indices (into {} (map-indexed #(vector %2 %1) headers))
             id-idx (get header-indices (:id column-mapping))
             label-idx (get header-indices (:label column-mapping))
             deps-idx (get header-indices (:dependencies column-mapping))
             lines (rest (str/split-lines csv-content))
             tasks (for [line lines
                        :let [cells (str/split line #",")
                              id (when id-idx (nth cells id-idx nil))
                              label (when label-idx (nth cells label-idx nil))
                              deps-str (when deps-idx (nth cells deps-idx nil))
                              dependencies (when deps-str
                                           (->> (str/split deps-str #"[;,|]")
                                                (map str/trim)
                                                ;; return only integers in text
                                                ;; TODO: handle differntly labelled deps, outside of CSV scope for now
                                                (map #(when (re-matches #"\d+" %)
                                                        (js/parseInt %)))
                                                (filter seq)
                                                vec))]
                        :when (and id (seq id))]
                    {:id (js/parseInt (str/trim id))
                     :label (or (when label (str/trim label)) (str/trim id))
                     :dependencies (or dependencies [])})]
         {:fx [[:dispatch [:csv/close-modal]]
               [:dispatch [:tasks/import-from-csv tasks]]]})))))

(rf/reg-event-db
 :tasks/import-from-csv
 (fn [db [_ imported-tasks]]
   ;; Merge with existing tasks or replace - adjust based on your needs
   (assoc db :app/tasks (vec imported-tasks))))

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
   ;; (println ":datagrid/rows")
   (clj->js tasks)))

(rf/reg-sub
 :csv/drag-over
 (fn [db _]
   (:csv/drag-over db)))

(rf/reg-sub
 :csv/modal-open
 (fn [db _]
   (:csv/modal-open db)))

(rf/reg-sub
 :csv/filename
 (fn [db _]
   (:csv/filename db)))

(rf/reg-sub
 :csv/headers
 (fn [db _]
   (:csv/headers db)))

(rf/reg-sub
 :csv/sample-rows
 (fn [db _]
   (:csv/sample-rows db)))

(rf/reg-sub
 :csv/column-mapping
 (fn [db _]
   (:csv/column-mapping db)))

(rf/reg-sub
 :csv/mapping-valid?
 :<- [:csv/column-mapping]
 (fn [mapping _]
   (and (:id mapping) (:label mapping))))

(defn handle-row-update [js-row]
  ;; TODO: validate row before dispatching
  (let [row (-> js-row
                (js->clj :keywordize-keys true)
                (update :dependencies set))]
    ;; (println "handle-row-update" row)
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
                              (let [dependencies (clj->js (parse-dependencies val))]
                                (set! (.-dependencies row) dependencies))
                              row)
               }])

(defui app []
  (let [selected-task (urf/use-subscribe [:app/selected-task])
        editing? (urf/use-subscribe [:ui/editing-text])
        undo? (urf/use-subscribe [:undos?])
        redo? (urf/use-subscribe [:redos?])
        last-undo (last (urf/use-subscribe [:undo-explanations]))
        last-redo (first (urf/use-subscribe [:redo-explanations]))
        ]

                                        ; delete key handling
    (uix/use-effect
     (fn []
       (let [handle-keydown
             (fn [event]
               (let [meta-key (or (.-metaKey event) (.-ctrlKey event))]
                                        ; undo / redo
                 (when (and meta-key (= (.-key event) "z"))
                   (do (.preventDefault event) (rf/dispatch [:undo])))
                 (when (and meta-key (= (.-key event) "Z"))
                   (do (.preventDefault event) (rf/dispatch [:redo])))
                                        ; delete / backspace
                 (when (or (= (.-key event) "Delete")
                           (= (.-key event) "Backspace"))
                   (cond (not editing?)
                         (do
                           (rf/dispatch [:ui/delete-selected])
                           (.preventDefault event))))))]
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
       ($ :div nil
          ($ :button {:onClick #(rf/dispatch [:ui/add-task])
                      :style {:margin "10px"}}
             "Add Task")
          (cond undo?
                ($ :button {:onClick #(rf/dispatch [:undo])
                            :style {:margin "10px"}}
                   (str "↶ undo " last-undo)))
          (cond redo?
                ($ :button {:onClick #(rf/dispatch [:redo])
                            :style {:margin "10px"}}
                   (str "↷ redo " last-redo))))
       ($ :div {:style {:display "block"
                        :height "50vh"
                        :width "100%"}}
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
                       }))
       ($ drop-zone nil)
       ($ csv-import-modal nil)
       )
    ))

(defonce root
  (uix.dom/create-root (js/document.getElementById "root")))

(defn init []
  (rf/dispatch-sync [:initialize-db])
  (uix.dom/render-root ($ app) root))

(defn ^:dev/after-load after-reload []
  (rf/clear-subscription-cache!)
  (uix.dom/render-root ($ app) root))
