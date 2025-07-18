(ns pert-canvas.ui.app
  (:require [clojure.string :as str]
            [clojure.set :refer [difference union]]
            [uix.core :as uix :refer [defui $]]
            [uix.dom]
            [uix.re-frame :as urf]
            [re-frame.core :as rf]
            ;; JS
            ["@xyflow/react" :refer [ReactFlow Background Controls]]
            ["@mui/x-data-grid" :refer [DataGrid]]
            ;; local
            [pert-canvas.utils :refer [deps-csv->set]]
            [pert-canvas.ui.events]
            [pert-canvas.ui.subs :as subs]
            [pert-canvas.ui.components.csv-drop-zone :refer [drop-zone]]
            [pert-canvas.ui.components.csv-import-modal :refer [csv-import-modal]]
            ))


;; Re-frame events and subscriptions
(defn handle-row-update [js-row]
  ;; TODO: validate row before dispatching
  (let [row (-> js-row
                (js->clj :keywordize-keys true)
                (update :dependencies set))]
    ;; (println "handle-row-update" row)
    (rf/dispatch [:ui/update-row row])
    (clj->js row)))


;; App
(def columns [{
               :field "id"
               :headerName "ID"
               :width 80
               ;; :maxWidth 150
               :resizable true
               }
              {
               :field "label"
               :headerName "Label"
               :editable true
               :resizable true
               :flex 1
               ;; :minWidth 100
               }
              ;; {:field "description" :headerName "Description" :editable true :width 300}
              {
               :field "dependencies"
               :headerName "Dependencies"
               :editable true
               ;; :width 100
               :maxWidth 300
               :valueGetter (fn [_, row] (str/join ", " (.. row -dependencies)))
               :valueSetter (fn [val, row]
                              (let [dependencies (clj->js (deps-csv->set val))]
                                (set! (.-dependencies row) dependencies))
                              row)}
              ])

(defui app []
  (let [selected-task (urf/use-subscribe [:app/selected-task])
        editing? (urf/use-subscribe [:ui/editing-text])
        undo? (urf/use-subscribe [:undos?])
        redo? (urf/use-subscribe [:redos?])
        last-undo (last (urf/use-subscribe [:undo-explanations]))
        last-redo (first (urf/use-subscribe [:redo-explanations]))]

    (uix/use-effect  ; delete key handling
     (fn []
       (let [handle-keydown
             (fn [event]
               (let [meta-key (or (.-metaKey event) (.-ctrlKey event))]
                                        ; undo / redo
                 (when (and meta-key (= (.-key event) "z"))
                   (do (.preventDefault event) (rf/dispatch [:undo])))
                 (when (and meta-key (= (.-key event) "Z"))
                   (do (.preventDefault event) (rf/dispatch [:redo])))
                                        ; delete / backspace
                 (when (or (= (.-key event) "Delete")
                           (= (.-key event) "Backspace"))
                   (cond (not editing?)
                         (do
                           (rf/dispatch [:ui/delete-selected])
                           (.preventDefault event))))))]
         (.addEventListener js/document "keydown" handle-keydown)
         ;; Cleanup function
         #(.removeEventListener js/document "keydown" handle-keydown)))
     [editing?])

    ($ :div
       {:on-drag-enter (fn [e]
                        (.preventDefault e)
                        (rf/dispatch [:csv/set-drag-over true]))
        :on-drag-over (fn [e]
                       (.preventDefault e))
        :on-drag-leave (fn [e]
                        (.preventDefault e)
                        (when (= (.-target e) (.-currentTarget e))
                          (rf/dispatch [:csv/set-drag-over false])))
        :on-drop (fn [e]
                  (.preventDefault e)
                  (let [files (-> e .-dataTransfer .-files)
                        file (when (> (.-length files) 0)
                              (aget files 0))]
                    (when (and file (re-find #"\.csv$" (.-name file)))
                      (rf/dispatch [:csv/file-dropped file]))
                    (rf/dispatch [:csv/set-drag-over false])))}
       ($ :h2
          {:style {:font "1.5em 'Fira Sans', sans-serif"}}
          "PERT Canvas")
       ($ :button {:onClick #(rf/dispatch [:ui/add-task])
                   :style {:margin "10px"}}
          "Add Task")
       (cond undo?
             ($ :button {:onClick #(rf/dispatch [:undo])
                         :style {:margin "10px"}}
                (str "↶ undo " last-undo)))
       (cond redo?
             ($ :button {:onClick #(rf/dispatch [:redo])
                         :style {:margin "10px"}}
                (str "↷ redo " last-redo)))

       ($ :div
          {:style {:display "flex"
                   :flex-direction "row"}}
          ($ :div
             {:style {:width "auto"
                      :maxWidth "40%"
                      :height "80vh"}}
             ($ DataGrid {
                          :rows (urf/use-subscribe [:datagrid/rows])
                          :columns (clj->js columns)
                          :processRowUpdate handle-row-update
                          :rowSelectionModel (clj->js [selected-task])
                          :onRowClick #(rf/dispatch [:ui/select-row (int (.-id %))] )
                          :onCellEditStart #(rf/dispatch [:ui/edit-row-start (int (.-id %))])
                          :onCellEditStop #(rf/dispatch [:ui/edit-row-end (int (.-id %))])
                          :onProcessRowUpdateError (fn [error] (print error))
                          }))
          ($ :div {:style {:height "80vh"
                           :flex "1 1 auto"}}
             ($ ReactFlow (urf/use-subscribe [:reactflow/config])
                ($ Background nil)
                ($ Controls nil))))
       ($ drop-zone nil)
       ($ csv-import-modal nil)
       )
    ))

(defonce root
  (uix.dom/create-root (js/document.getElementById "root")))

(defn init []
  (rf/dispatch-sync [:initialize-db])
  (uix.dom/render-root ($ app) root))

(defn ^:dev/after-load after-reload []
  (rf/clear-subscription-cache!)
  (uix.dom/render-root ($ app) root))

(comment
  (:csv/headers @re-frame.db/app-db)
  @re-frame.db/app-db
  (get-in @re-frame.db/app-db [:app/tasks])
  @(rf/subscribe [:datagrid/rows])
  )
