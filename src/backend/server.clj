(ns server
  (:require [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.middleware.cors :refer [wrap-cors]]
            [compojure.core :refer [defroutes GET POST PUT DELETE ANY context]]
            [compojure.route :as route]
            [clj-http.client :as http]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(def secrets (edn/read-string (slurp ".secret")))

(def planio-config
  {:base-url (get-in secrets [:planio :base-url])
   :auth [(get-in secrets [:planio :username])
          (get-in secrets [:planio :password])]})

(defn proxy-to-planio [request path]
  (let [method (:request-method request)
        url (str (:base-url planio-config) path)
        query-string (:query-string request)
        full-url (if query-string (str url "?" query-string) url)
        headers (dissoc (:headers request) "host" "content-length")
        body (:body request)]
    
    (try
      (let [request-opts {:method method
                          :url full-url
                          :basic-auth (:auth planio-config)
                          :headers (merge headers {"Accept" "application/json"})
                          :as :json
                          :throw-exceptions false}
            request-opts (if (and body (not (instance? java.io.InputStream body)))
                          (assoc request-opts :body (json/generate-string body))
                          request-opts)
            response (http/request request-opts)]
        
        {:status (:status response)
         :headers {"Content-Type" "application/json"
                   "Access-Control-Allow-Origin" "*"}
         :body (:body response)})
      
      (catch Exception e
        {:status 500
         :headers {"Content-Type" "application/json"}
         :body {:error "Proxy request failed" :message (.getMessage e)}}))))

(defroutes app-routes
  (context "/api" []
    (ANY "*" request
      (let [path (str/replace (:uri request) #"^/api" "")]
        (proxy-to-planio request path))))
  
  (route/not-found {:status 404 :body {:error "Not found"}}))

(def app
  (-> app-routes
      (wrap-cors :access-control-allow-origin [#".*"]
                 :access-control-allow-methods [:get :post :put :delete :options]
                 :access-control-allow-headers ["Content-Type" "Authorization"])
      wrap-json-body
      wrap-json-response))

(defn -main [& args]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "3000"))]
    (println (str "Starting server on port " port))
    (run-jetty app {:port port :join? true})))

(comment
  ;; Start server for development
  (def server (run-jetty app {:port 3000 :join? false}))
  ;; Stop server
  (.stop server))
