(ns pert-canvas.ui.db
  (:require [datascript.core :as d]))

;; DataScript schema for tasks
(def schema
  {:task/id {:db/unique :db.unique/identity}
   :task/label {:db/type :db.type/string}
   :task/description {:db/type :db.type/string}
   :task/dependency {:db/valueType :db.type/ref
                     :db/cardinality :db.cardinality/many}})

;; Initial task data
(def initial-tasks-data
  [{:task/id 1
    :task/label "Buy Ingredients"
    :task/description "Buy Ingredients"}
   {:task/id 2
    :task/label "Mix Ingredients"
    :task/description "Mix Ingredients"}
   {:task/id 3
    :task/label "place dough on pan"
    :task/description ""}
   {:task/id 4
    :task/label "Bake dough"
    :task/description ""}
   {:task/id 5
    :task/label "Preheat Oven"
    :task/description ""}
   {:task/id 6
    :task/label "Eat Cookies"
    :task/description ""}])

;; Dependencies as separate datoms (will be added after entities exist)
(def initial-dependencies
  [[2 1]   ; Mix depends on Buy
   [3 2]   ; Place depends on Mix
   [4 3]   ; Bake depends on Place
   [4 5]   ; Bake depends on Preheat
   [6 4]]) ; Eat depends on Bake

(defn create-conn
  "Create a new DataScript connection with initial data"
  ([]
   (let [conn (d/create-conn schema)]
     ;; Add tasks
     (d/transact! conn initial-tasks-data)
     ;; Add dependencies (using lookup refs)
     (d/transact! conn
                  (mapv (fn [[task-id dep-id]]
                          {:task/id task-id
                           :task/dependency [:task/id dep-id]})
                        initial-dependencies))
     conn))
  ([db-val]
   ;; Create a conn from an existing DB value (for undo/redo)
   (d/conn-from-db db-val)))

;; Helper functions for converting between DataScript and app formats

(defn task-entity->map
  "Convert a DataScript task entity to the app's task map format"
  [entity]
  (when entity
    {:id (:task/id entity)
     :label (:task/label entity)
     :description (or (:task/description entity) "")
     :dependencies (into #{} (map :task/id (:task/dependency entity)))}))

(defn all-tasks
  "Get all tasks from the DB as a vector of maps"
  [db]
  (->> (d/q '[:find [?e ...]
              :where [?e :task/id]]
            db)
       (map #(d/entity db %))
       (map task-entity->map)
       (sort-by :id)
       vec))

(defn task-by-id
  "Get a single task by ID"
  [db task-id]
  (when-let [entity (d/entity db [:task/id task-id])]
    (task-entity->map entity)))

(defn task-edges
  "Get all dependency edges as a vector of maps for ReactFlow"
  [db]
  (vec
   (for [[task-id dep-id]
         (d/q '[:find ?tid ?did
                :where
                [?t :task/id ?tid]
                [?t :task/dependency ?d]
                [?d :task/id ?did]]
              db)]
     {:id (str "edge-" dep-id "-" task-id)
      :source (str dep-id)
      :target (str task-id)})))

(defn next-task-id
  "Get the next available task ID"
  [db]
  (inc (or (d/q '[:find (max ?id) .
                  :where [_ :task/id ?id]]
                db)
           0)))

;; Transaction helper functions

(defn add-task-tx
  "Create transaction data for adding a new task"
  [db label description]
  (let [new-id (next-task-id db)]
    [{:task/id new-id
      :task/label label
      :task/description description}]))

(defn update-task-tx
  "Create transaction data for updating a task"
  [task-id updates]
  [(merge {:task/id task-id}
          (when-let [label (:label updates)]
            {:task/label label})
          (when-let [desc (:description updates)]
            {:task/description desc}))])

(defn delete-task-tx
  "Create transaction data for deleting a task and its relationships"
  [db task-id]
  (let [entity-id (d/q '[:find ?e .
                         :in $ ?id
                         :where [?e :task/id ?id]]
                       db task-id)]
    (when entity-id
      [[:db/retractEntity entity-id]])))

(defn add-dependency-tx
  "Create transaction data for adding a dependency"
  [task-id dependency-id]
  [{:task/id task-id
    :task/dependency [:task/id dependency-id]}])

(defn remove-dependency-tx
  "Create transaction data for removing a dependency"
  [db task-id dependency-id]
  (let [task-eid (d/q '[:find ?e .
                        :in $ ?id
                        :where [?e :task/id ?id]]
                      db task-id)
        dep-eid (d/q '[:find ?e .
                       :in $ ?id
                       :where [?e :task/id ?id]]
                     db dependency-id)]
    (when (and task-eid dep-eid)
      [[:db/retract task-eid :task/dependency dep-eid]])))

(defn replace-all-tasks-tx
  "Create transaction data for replacing all tasks (for CSV/Planio import)"
  [db new-tasks]
  ;; First, retract all existing tasks
  (let [existing-eids (d/q '[:find [?e ...]
                             :where [?e :task/id]]
                           db)
        retract-txs (mapv (fn [eid] [:db/retractEntity eid]) existing-eids)
        ;; Convert new tasks to DataScript format
        task-txs (mapv (fn [{:keys [id label description]}]
                         {:task/id id
                          :task/label label
                          :task/description (or description "")})
                       new-tasks)
        ;; Add dependencies after tasks exist
        dep-txs (mapcat (fn [{:keys [id dependencies]}]
                          (map (fn [dep-id]
                                 {:task/id id
                                  :task/dependency [:task/id dep-id]})
                               dependencies))
                        new-tasks)]
    (concat retract-txs task-txs dep-txs)))
