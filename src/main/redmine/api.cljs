(ns redmine.api
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.string :as str]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! chan]]
            [cljs.pprint :as pprint]))

;; Configuration
(def ^:dynamic *api-base-url* "http://localhost:3000/api")

(defn set-api-base-url! [url]
  (set! *api-base-url* url))

;; HTTP helpers
(defn api-url [path]
  (str *api-base-url* path))

(defn http-get [url & [options]]
  (http/get url (merge {:timeout 30000 :with-credentials? false} options)))

(defn http-post [url data & [options]]
  (http/post url (merge {:json-params data :timeout 30000} options)))

;; Issue fetching
(defn fetch-issues
  "Fetch issues with optional filters. Returns a channel with the response."
  [& {:keys [project-id parent-id status limit offset]}]
  (go
    (let [params (cond-> {}
                   project-id (assoc :project_id project-id)
                   parent-id (assoc :parent_id parent-id)
                   status (assoc :status_id status)
                   limit (assoc :limit limit)
                   offset (assoc :offset offset))
          query-string (when (seq params)
                        (->> params
                             (map (fn [[k v]] (str (name k) "=" v)))
                             (str/join "&")))
          url (api-url (str "/issues.json" (when query-string (str "?" query-string))))
          response (<! (http-get url))]
      
      (if (:success response)
        {:success true 
         :issues (get-in response [:body :issues])
         :total-count (get-in response [:body :total_count])}
        {:success false 
         :error (:error response)}))))

(defn fetch-issue
  "Fetch a single issue by ID. Returns a channel with the response."
  [issue-id & {:keys [include]}]
  (go
    (let [query-params (when include {:include (str/join "," (map name include))})
          query-string (when query-params
                        (->> query-params
                             (map (fn [[k v]] (str (name k) "=" v)))
                             (str/join "&")))
          url (api-url (str "/issues/" issue-id ".json" (when query-string (str "?" query-string))))
          response (<! (http-get url))]
      
      (if (:success response)
        {:success true 
         :issue (get-in response [:body :issue])}
        {:success false 
         :error (:error response)}))))

;; Issue transformation for PERT
(defn issue->task
  "Transform a Redmine issue into a PERT task structure."
  [issue]
  {:id (:id issue)
   :name (:subject issue)
   :duration (or (:estimated_hours issue) 0)
   :dependencies (when-let [relations (:relations issue)]
                   (->> relations
                        (filter #(= (:relation_type %) "precedes"))
                        (map :issue_to_id)))
   :status (:status issue)
   :assigned-to (get-in issue [:assigned_to :name])
   :due-date (:due_date issue)
   :done-ratio (:done_ratio issue)
   :description (:description issue)
   :project-id (get-in issue [:project :id])
   :tracker (:tracker issue)})

(defn fetch-project-tasks
  "Fetch all tasks for a project and return them in PERT format."
  [project-id]
  (go
    (let [response (<! (fetch-issues :project-id project-id :include [:relations]))
          issues (when (:success response) (:issues response))]
      
      (if (:success response)
        {:success true
         :tasks (map issue->task issues)}
        response))))

;; Projects
(defn fetch-projects
  "Fetch available projects. Returns a channel with the response."
  []
  (go
    (let [response (<! (http-get (api-url "/projects.json")))]
      
      (if (:success response)
        {:success true 
         :projects (get-in response [:body :projects])}
        {:success false 
         :error (:error response)}))))

;; Users
(defn fetch-users
  "Fetch project users. Returns a channel with the response."
  [project-id]
  (go
    (let [response (<! (http-get (api-url (str "/projects/" project-id "/memberships.json"))))]
      
      (if (:success response)
        {:success true 
         :users (->> (get-in response [:body :memberships])
                    (map :user))}
        {:success false 
         :error (:error response)}))))

;; Development/testing helpers
(comment
  ;; Test fetching issues
  (go (let [result (<! (fetch-issues :limit 5))]
        (pprint/pprint result)))
  
  ;; Test fetching a specific issue with relations
  (go (let [result (<! (fetch-issue 3133 :include [:relations]))]
        (pprint/pprint result)))
  
  ;; Test fetching project tasks
  (go (let [result (<! (fetch-project-tasks 123))]
        (pprint/pprint result))))
