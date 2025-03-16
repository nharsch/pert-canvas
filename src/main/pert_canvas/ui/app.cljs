(ns pert-canvas.ui.app
  (:require [uix.core :as uix :refer [defui $]]
            [uix.dom]
            ;; [uix.re-frame :as urf]
            ;; [reagent.core :as r]
            ;; [reaflow :refer [Canvas]]
            ["@xyflow/react" :refer [ReactFlow Background Controls]]
            ))


(defui app []
  ($ :div {:style {:height "100vh" :width "100vh"}}
   ($ :h1 nil "Hello, world!")
   ($ :div {:style {:height "100%"}}
      ($ ReactFlow {:nodes (clj->js [{:id "1" :position {:x 0 :y 0} :data {:label "Node 1"}}])}
         ($ Background nil)
         ($ Controls nil)))))


(defonce root
  (uix.dom/create-root (js/document.getElementById "root")))


(defn ^:dev/after-load init []
  (js/console.log "Hello, world!")
  (uix.dom/render-root ($ app) root))
