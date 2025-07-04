(ns pert-canvas.ui.components.csv-import-modal
  (:require [uix.core :as uix :refer [defui $]]
            [re-frame.core :as rf]
            [uix.re-frame :as urf]
            ["@mui/material" :as mui]))

(defui column-selector [{:keys [field label current-value headers on-change]}]
  ($ mui/FormControl
     {:fullWidth true :margin "normal"}
     ($ mui/InputLabel label)
     ($ mui/Select
        {:value (or current-value "")
         :label label
         :onChange #(on-change field (.. % -target -value))}
        ($ mui/MenuItem {:value ""} "-- Select Column --")
        (for [header headers]
          ($ mui/MenuItem {:key header :value header} header)))))

(defui sample-data-preview [{:keys [headers sample-rows]}]
  ($ mui/Paper
     {:sx {:p 2 :mt 2}}
     ($ mui/Typography {:variant "subtitle2" :gutterBottom true}
        "Sample Data Preview:")
     ($ mui/TableContainer
        ($ mui/Table {:size "small"}
           ($ mui/TableHead
              ($ mui/TableRow
                 (for [header headers]
                   ($ mui/TableCell {:key header} header))))
           ($ mui/TableBody
              (for [row sample-rows
                    :let [cells (clojure.string/split row #",")]]
                ($ mui/TableRow {:key row}
                   (for [[idx cell] (map-indexed vector cells)]
                     ($ mui/TableCell {:key idx}
                        (clojure.string/trim cell))))))))))











(defui csv-import-modal []
  (let [open? (urf/use-subscribe [:csv/modal-open])
        filename (urf/use-subscribe [:csv/filename])
        headers (urf/use-subscribe [:csv/headers])
        sample-rows (urf/use-subscribe [:csv/sample-rows])
        column-mapping (urf/use-subscribe [:csv/column-mapping])
        mapping-valid? (urf/use-subscribe [:csv/mapping-valid?])]

    ($ mui/Dialog
       {:open (boolean open?)
        :maxWidth "md"
        :fullWidth true
        }
       ($ mui/DialogTitle
          (str "Import CSV: " filename))
       ($ mui/DialogContent
          ($ mui/DialogContentText
             "Please map your CSV columns to the required task properties:")

          ($ column-selector
             {:field :id
              :label "Task ID Column"
              :current-value (:id column-mapping)
              :headers headers
              :on-change #(rf/dispatch [:csv/set-column-mapping %1 %2])})

          ($ column-selector
             {:field :label
              :label "Task Label Column"
              :current-value (:label column-mapping)
              :headers headers
              :on-change #(rf/dispatch [:csv/set-column-mapping %1 %2])})

          ($ column-selector
             {:field :dependencies
              :label "Dependencies Column (optional)"
              :current-value (:dependencies column-mapping)
              :headers headers
              :on-change #(rf/dispatch [:csv/set-column-mapping %1 %2])})

          ($ mui/Typography
             {:variant "caption" :sx {:mt 1 :display "block"}}
             "Dependencies should be separated by commas, semicolons, or pipes (|)")

          (when (and headers sample-rows)
            ($ sample-data-preview
               {:headers headers
                :sample-rows sample-rows})))

       ($ mui/DialogActions
          ($ mui/Button
             {:onClick #(rf/dispatch [:csv/close-modal])}
             "Cancel")
          ($ mui/Button
             {:onClick #(rf/dispatch [:csv/import-tasks])
              :disabled (not mapping-valid?)
              :variant "contained"}
             "Import Tasks")))))
