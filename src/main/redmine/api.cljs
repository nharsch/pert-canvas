(ns redmine.api
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.string :as str]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [cljs.pprint :as pprint]))


(def test-url "https://thelabnyc.plan.io/issues.json?include=relations&utf8=%E2%9C%93&set_filter=1&sort=id%3Adesc&f%5B%5D=status_id&op%5Bstatus_id%5D=o&f%5B%5D=parent_id&op%5Bparent_id%5D=%7E&v%5Bparent_id%5D%5B%5D=29462&f%5B%5D=&c%5B%5D=project&c%5B%5D=tracker&c%5B%5D=status&c%5B%5D=priority&c%5B%5D=cf_12&c%5B%5D=subject&c%5B%5D=category&c%5B%5D=fixed_version&c%5B%5D=assigned_to&c%5B%5D=updated_on&c%5B%5D=due_date&c%5B%5D=estimated_hours&c%5B%5D=done_ratio&c%5B%5D=relations&group_by=fixed_version&t%5B%5D=")

(go (let [response (<! (http/get test-url
                                 {:headers {}
                                  :with-credentials? false
                                  :timeout 80000}))]
      (pprint (map #(select-keys % [:id :relations :subject])
                  (:issues (:body response))))))
