(ns redmine.api
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [clojure.string :as str]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! <p!]]))

;; Configuration
(def ^:private default-config
  {:base-url "https://thelabnyc.plan.io"
   :api-key nil
   :format "json"
   :timeout 10000
   :oauth {:client-id nil
           :client-secret nil
           :redirect-uri "urn:ietf:wg:oauth:2.0:oob"
           :scope "add_issues view_issues"
           :access-token nil
           :refresh-token nil
           :token-expires-at nil}})

