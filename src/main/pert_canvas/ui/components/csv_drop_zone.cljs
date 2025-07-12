(ns pert-canvas.ui.components.csv-drop-zone
  (:require [uix.core :as uix :refer [defui $]]
            [re-frame.core :as rf]
            [uix.re-frame :as urf]))

(defui drop-zone []
  (let [drag-over? (urf/use-subscribe [:csv/drag-over])]
    ($ :div
       {:style {:position "fixed"
                :top 0
                :left 0
                :right 0
                :bottom 0
                :pointer-events "none"
                :background-color (if drag-over? "rgba(0, 0, 0, 0.5)" "transparent")
                :z-index (if drag-over? 9999 -1)
                :display "flex"
                :align-items "center"
                :justify-content "center"
                :transition "all 0.2s ease"}}
       (when drag-over?
         ($ :h1
            {:style {:background-color "white"
                     :border "3px dashed #ccc"
                     :border-radius "10px"
                     :padding "40px"
                     :text-align "center"
                     :font-size "18px"
                     :color "#666"
                     :pointer-events "none"}}
            "Drop CSV file here to import tasks")))))
