(ns pert-canvas.ui.components.csv-drop-zone
  (:require [uix.core :as uix :refer [defui $]]
            [re-frame.core :as rf]
            [uix.re-frame :as urf]))

(defui drop-zone []
  (let [drag-over? (urf/use-subscribe [:csv/drag-over])
        container-ref (uix/use-ref nil)]
    ($ :div
       {:ref container-ref
        :style {:position "fixed"
                :top 0
                :left 0
                :right 0
                :bottom 0
                ;; :pointer-events (if drag-over? "auto" "none")
                :background-color (if drag-over? "rgba(0, 0, 0, 0.5)" "transparent")
                :z-index (if drag-over? 9999 -1)
                :display "flex"
                :align-items "center"
                :justify-content "center"
                :transition "all 0.2s ease"}
        :on-drag-enter (fn [e]
                        (.preventDefault e)
                        (println "Drag enter")
                        (rf/dispatch [:csv/set-drag-over true]))
        :on-drag-over (fn [e]
                        ;; (println "Drag over")
                       (.preventDefault e))
        :on-drag-leave (fn [e]
                        (.preventDefault e)
                        (println "Drag leave")
                        (when (= (.-target e) (.-currentTarget e))
                          (rf/dispatch [:csv/set-drag-over false])
                          )
                         )
        :on-drop (fn [e]
                  (.preventDefault e)
                  (println "File dropped")
                  (let [files (-> e .-dataTransfer .-files)
                        file (when (> (.-length files) 0)
                              (aget files 0))]
                    (when (and file (re-find #"\.csv$" (.-name file)))
                      (rf/dispatch [:csv/file-dropped file]))
                    (rf/dispatch [:csv/set-drag-over false])))}
       (when drag-over?
         ($ :div
            {:style {:background-color "white"
                     :border "3px dashed #ccc"
                     :border-radius "10px"
                     :padding "40px"
                     :text-align "center"
                     :font-size "18px"
                     :color "#666"}}
            "Drop CSV file here to import tasks")))))
