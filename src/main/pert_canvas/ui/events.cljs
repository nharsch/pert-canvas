(ns pert-canvas.ui.events
  (:require
   [re-frame.core :as rf]
   [day8.re-frame.undo :refer [undoable]]
   [clojure.set :refer [union]]
   [clojure.string :as str]
   [cljs.core.async :refer [<!]]
   [goog.labs.format.csv :as csv]
   [malli.core :as m]
   [malli.error :as me]
   [pert-canvas.utils :refer [keywordize-values
                              csv->tasks
                              remove-dep-from-row
                              edgeid->ids]]
   [pert-canvas.ui.state :refer [initial-state state-task state-tasks]]
   [redmine.api :as redmine])
  (:require-macros [cljs.core.async.macros :refer [go]]))


;; TODO: move to a separate state namespace?
(defn delete-task-from-db
  [db task-id]
  ;; (println "delete-task" task-id)
  (let [new-tasks
        (update db :app/tasks
                (fn [tasks]
                  (remove #(= (:id %) (int task-id)) tasks)))]
    new-tasks))

(defn delete-edge-from-db
  [db edge-id]
  ;; (println "delete-edge" edge-id)
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
 ;; (println ":initialize-db")
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
   (println "create-connection" source-id target-id)
   (update-in db [:app/tasks]
              (fn [tasks]
                (map #(if (= (:id %) (int target-id))
                        (do
                          (println "found dep for" target-id source-id)
                          (update % :dependencies union #{(int source-id)}))
                        %) tasks)))))

(rf/reg-event-db
 :ui/update-row
 (undoable "update task row")
 (fn [db [_ row]]
   (println "update-row" row)
   (if
       (m/validate state-task row)
       (update-in db [:app/tasks]
                  (fn [tasks]
                    (map #(if (= (:id %) (:id row)) row %) tasks)))
       (println (:errors (m/explain state-task row))))))

(rf/reg-event-db
 :reactflow/nodes-dims-calc
 (fn [db [_ nodes]]
   (assoc db :reactflow/nodes-dims nodes)))

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
 (undoable "import tasks from CSV")
 (fn [db [_ imported-tasks]]
   (if
       (m/validate state-tasks imported-tasks)
       ;; Merge with existing tasks or replace - adjust based on your needs
       (assoc db :app/tasks imported-tasks)
       ;; TODO: throw error ealier if possible (during modal open)
       ;; TODO: toast error or some alert
       (println "CSV import errors: " (:errors (m/explain state-tasks imported-tasks)))
       )))

;; Plan.io URL handling
(defn add-relations-include
  "Add includes=relations to query string if not already present"
  [query-string]
  (if (str/includes? query-string "include")
    ;; Already has includes, check if relations is there
    (if (str/includes? query-string "relations")
      query-string
      ;; Add relations to existing includes
      (str/replace query-string #"include=([^&]*)" "include=$1,relations"))
    ;; No includes parameter, add it
    (if (empty? query-string)
      "?include=relations"
      (str query-string "&include=relations"))))

(defn normalize-planio-path
  "Convert regular Plan.io paths to JSON API paths"
  [path]
  (cond
    ;; /issues -> /issues.json
    (= path "/issues")
    "/issues.json"
    
    ;; /issues/123 -> /issues/123.json  
    (re-matches #"^/issues/\d+$" path)
    (str path ".json")
    
    ;; /projects -> /projects.json
    (= path "/projects")
    "/projects.json"
    
    ;; /projects/123 -> /projects/123.json
    (re-matches #"^/projects/\d+$" path)
    (str path ".json")
    
    ;; Already has .json extension, leave as-is
    (str/includes? path ".json")
    path
    
    ;; Other paths, assume they need .json
    :else
    (if (str/includes? path ".")
      path  ; Already has an extension, leave it
      (str path ".json"))))

(defn extract-path-from-planio-url 
  "Extract the path from a full Plan.io URL for API calls and ensure relations are included"
  [url]
  (try
    (let [url-obj (js/URL. url)
          path (.-pathname url-obj)
          search (.-search url-obj)
          normalized-path (normalize-planio-path path)
          enhanced-search (add-relations-include search)]
      (str normalized-path enhanced-search))
    (catch js/Error _
      ;; If it's not a valid URL, assume it's already a path
      (let [normalized-path (normalize-planio-path url)]
        (add-relations-include normalized-path)))))

(rf/reg-event-db
 :planio/set-url
 (fn [db [_ url]]
   (assoc db :planio/url url)))

(rf/reg-event-db
 :planio/set-loading
 (fn [db [_ loading?]]
   (assoc db :planio/loading loading?)))

(rf/reg-event-fx
 :planio/fetch-from-url
 (fn [{:keys [db]} [_ url]]
   (let [path (extract-path-from-planio-url url)]
     (println "Fetching from Plan.io path:" path)
     {:db (assoc db :planio/loading true)
      :fx [[:dispatch [:planio/make-api-request path]]]})))

(rf/reg-event-fx
 :planio/make-api-request
 (fn [{:keys [db]} [_ path]]
   (go
     (let [response (<! (redmine/http-get (redmine/api-url path)))]
       (if (:success response)
         (rf/dispatch [:planio/fetch-success (:body response)])
         (rf/dispatch [:planio/fetch-error (:error response)]))))
   {}))

(rf/reg-event-db
 :planio/fetch-success
 (fn [db [_ response-data]]
   (println "Plan.io fetch success:" response-data)
   (-> db
       (assoc :planio/loading false)
       (assoc :planio/last-response response-data))))

(rf/reg-event-db
 :planio/fetch-error
 (fn [db [_ error]]
   (println "Plan.io fetch error:" error)
   (-> db
       (assoc :planio/loading false)
       (assoc :planio/error error))))

;; Plan.io to PERT conversion
(defn extract-all-relations
  "Extract all relations from all issues in the response"
  [issues]
  (mapcat :relations issues))

(defn build-dependencies-map
  "Build a map of issue-id -> set of dependency issue-ids from relations"
  [all-relations]
  (reduce (fn [deps-map relation]
            (if (= (:relation_type relation) "blocks")
              (let [dependency (:issue_id relation)     ; Issue that blocks
                    dependent (:issue_to_id relation)]  ; Issue that is blocked
                ;; dependent depends on dependency
                (update deps-map dependent (fnil conj #{}) dependency))
              deps-map))
          {}
          all-relations))

(defn planio-issue->pert-task
  "Convert a Plan.io issue to PERT task format"
  [issue dependencies-map]
  {:id (:id issue)
   :label (str (:subject issue))  ; Ensure it's a string
   :description (str (or (:description issue) ""))  ; Ensure it's a string, handle nil
   :dependencies (get dependencies-map (:id issue) #{})})

(defn planio-response->pert-tasks
  "Convert a Plan.io issues response to PERT tasks format"
  [response-data]
  (when-let [issues (:issues response-data)]
    (let [all-relations (extract-all-relations issues)
          dependencies-map (build-dependencies-map all-relations)
          issue-ids (set (map :id issues))
          ;; Filter dependencies to only include issues that exist in this result set
          filtered-deps-map (into {} 
                                  (map (fn [[issue-id deps]]
                                         [issue-id (set (filter issue-ids deps))])
                                       dependencies-map))]
      ;; Use vec to ensure we return a vector, not a lazy sequence
      (vec (map #(planio-issue->pert-task % filtered-deps-map) issues)))))

(rf/reg-event-fx
 :planio/convert-to-pert-tasks
 (fn [{:keys [db]} _]
   (if-let [response-data (:planio/last-response db)]
     (let [pert-tasks (planio-response->pert-tasks response-data)]
       (println "Converted to PERT tasks:" (count pert-tasks) "tasks")
       {:fx [[:dispatch [:tasks/import-from-planio pert-tasks]]]})
     (println "No Plan.io response data to convert"))))

(rf/reg-event-db
 :tasks/import-from-planio
 (undoable "import tasks from Plan.io")
 (fn [db [_ imported-tasks]]
   (println "Importing tasks:" (count imported-tasks) "tasks")
   (println "First task sample:" (first imported-tasks))
   (if (m/validate state-tasks imported-tasks)
     (do
       (println "Tasks validated successfully, importing...")
       (assoc db :app/tasks imported-tasks))
     (do
       (println "Plan.io import validation errors:")
       (println (m/explain state-tasks imported-tasks))
       db))))
