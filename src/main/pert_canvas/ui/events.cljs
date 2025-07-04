(ns pert-canvas.ui.events
  (:require
   [re-frame.core :as rf]
   [day8.re-frame.undo :refer [undoable]]
   [clojure.set :refer [union]]
   [goog.labs.format.csv :as csv]
   [malli.core :as m]
   [malli.error :as me]
   [pert-canvas.utils :refer [keywordize-values
                              csv->tasks
                              remove-dep-from-row
                              edgeid->ids]]
   [pert-canvas.ui.state :refer [initial-state state-task]]))


;; TODO: move to a separate state namespace?
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
   ;; TODO use CSV parser for this
   (let [lines (csv/parse csv-content)
         headers (first lines)
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



;; TODO: move to a separate namespace
(rf/reg-event-fx
 :csv/import-tasks
 (fn [{:keys [db]} _]
   (println "csv/import-tasks")
   (let [csv-content (:csv/raw-content db)
         column-mapping (keywordize-values (:csv/column-mapping db))
         headers (:csv/headers db)]
     (when (and csv-content column-mapping headers)
       (let [tasks (csv->tasks csv-content column-mapping)]
         (println "column-mapping:" column-mapping)
         (println "Parsed tasks:" (:dependencies (first tasks)))
         {:fx [[:dispatch [:csv/close-modal]]
               [:dispatch [:tasks/import-from-csv tasks]]]}
         )))))


(rf/reg-event-db
 :tasks/import-from-csv
 (fn [db [_ imported-tasks]]
   ;; Merge with existing tasks or replace - adjust based on your needs
   (assoc db :app/tasks (vec imported-tasks))))
