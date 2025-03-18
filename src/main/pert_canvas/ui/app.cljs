(ns pert-canvas.ui.app
  (:require [uix.core :as uix :refer [defui $]]
            [uix.dom]
            ;; [uix.re-frame :as urf]
            ;; [reagent.core :as r]
            ;; [reaflow :refer [Canvas]]
            ["@xyflow/react" :refer [ReactFlow Background Controls]]
            ["@dagrejs/dagre" :as Dagre]
            ["@mui/x-data-grid" :refer [DataGrid]]
            ))

(def initial-state
  {:nodes [{:id "1"
            :position {:x 0 :y 0}
            :data {:label "Node 1"}}
           {:id "2"
            :position {:x 0 :y 100}
            :data {:label "Node 2"}}
           {:id "3"
            :position {:x 100 :y 200}
            :data {:label "Node 3"}}
           {:id "4"
            :position {:x 200 :y 100}
            :data {:label "Node 4"}}
           {:id "5"
            :position {:x 200 :y 0}
            :data {:label "Node 5"}}
           ]
   :edges [
           {:id 1 :source "1" :target "2"}
           {:id 2 :source "2" :target "3"}
           {:id 3 :source "2" :target "4"}
           {:id 4 :source "3" :target "5"}
           ]})

(defn add-source-position-lr [node]
                       (merge node {:sourcePosition "right"
                                    :targetPosition "left"}))


(get-in {:id 0} [:measured :width] 0)

;; TODO: move to ui/layout.cljs
(defn get-layouted-elements [nodes edges]
  (let [g (-> (new (.. Dagre -graphlib -Graph))
              (.setDefaultEdgeLabel (fn [] #js {})))]
    (.setGraph g #js {:rankdir "LR"})

    (doseq [edge edges]
      (.setEdge g (:source edge) (:target edge)))

    (doseq [node nodes]
      (.setNode g (:id node)
                (clj->js (merge node
                                {:width (get-in node [:measured :width] 150)
                                 :height (get-in node [:measured :height] 0)}))))

    (.layout Dagre g)

    {:nodes (map (fn [node]
                   (let [position (.node g (:id node))
                         width (get-in node [:measured :width] 150)
                         height (get-in node [:measured :height] 0)
                         x (- (.-x position) (/ width 2))
                         y (- (.-y position) (/ height 2))]
                     (assoc node :position {:x x :y y})))
                 nodes)
     :edges edges}))

(defui app []
  ($ :div {:style {:height "60vh" :width "100%"}}
     ($ :h1 nil "Hello, world!")
     ($ :div {:style {:height "60vh"}}
        ($ ReactFlow (clj->js (get-layouted-elements (map add-source-position-lr (:nodes initial-state)) (:edges initial-state)))
           ($ Background nil)
           ($ Controls nil)))
     ($ :div {:style {:display "block"
                      :height "40vh"
                      :width "100%"}}
        ($ DataGrid {:rows (clj->js (map (fn [node]
                                           (assoc node :id (str (:id node))))
                                         (:nodes initial-state)))
                     :columns (clj->js [{:field "id" :headerName "ID" :width 100}
                                        {:field "data.label" :headerName "Label" :width 200}])}))))


(defonce root
  (uix.dom/create-root (js/document.getElementById "root")))


(defn ^:dev/after-load init []
  (js/console.log "Hello, world!")
  (uix.dom/render-root ($ app) root))
