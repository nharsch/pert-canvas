(ns proxy
  (:require [clojure.string :as str]))

(defn api-proxy? [exchange config]
  (println "api-proxy? URI" (.getRequestURI exchange))
  (or
   (str/starts-with? (.getRequestURI exchange) "/issues")
   (str/starts-with? (.getRequestURI exchange) "/login")
   )
  )
