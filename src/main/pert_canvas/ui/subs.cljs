(ns pert-canvas.ui.subs
    (:require
     [re-frame.core :as rf]
     [pert-canvas.utils :refer [get-layouted-nodes state-task->canvas-node]]))

(def mark-nodes-selected
  (memoize
   (fn [selected-id nodes]
     (map #(if (= (:id %) (int selected-id))
             (assoc % :selected true)
             %)
          nodes))))

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
