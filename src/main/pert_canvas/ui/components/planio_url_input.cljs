(ns pert-canvas.ui.components.planio-url-input
  (:require [uix.core :as uix :refer [defui $]]
            [uix.re-frame :as urf]
            [re-frame.core :as rf]))

(defui planio-url-input []
  (let [url (urf/use-subscribe [:planio/url])
        loading? (urf/use-subscribe [:planio/loading])
        error (urf/use-subscribe [:planio/error])
        last-response (urf/use-subscribe [:planio/last-response])
        [local-url set-local-url!] (uix/use-state (or url ""))]

    ($ :div
       {:style {:margin "10px 0"
                :padding "10px"
                :border "1px solid #ddd"
                :border-radius "4px"}}
       
       ($ :h3 {:style {:margin "0 0 10px 0"}} "Import from Plan.io")
       
       ($ :div
          {:style {:display "flex"
                   :gap "10px"
                   :align-items "center"}}
          
          ($ :input
             {:type "text"
              :placeholder "Enter Plan.io URL (e.g., https://yoursite.plan.io/issues.json)"
              :value local-url
              :onChange #(set-local-url! (.. % -target -value))
              :onKeyPress (fn [e]
                           (when (= (.-key e) "Enter")
                             (rf/dispatch [:planio/fetch-from-url local-url])))
              :style {:flex "1"
                      :padding "8px"
                      :border "1px solid #ccc"
                      :border-radius "4px"}
              :disabled loading?})
          
          ($ :button
             {:onClick #(rf/dispatch [:planio/fetch-from-url local-url])
              :disabled (or loading? (empty? local-url))
              :style {:padding "8px 16px"
                      :background (if loading? "#ccc" "#007bff")
                      :color "white"
                      :border "none"
                      :border-radius "4px"
                      :cursor (if loading? "not-allowed" "pointer")}}
             (if loading? "Loading..." "Fetch")))
       
       ;; Status messages
       (when loading?
         ($ :div {:style {:margin-top "10px" :color "#007bff"}}
            "Fetching data from Plan.io..."))
       
       (when error
         ($ :div {:style {:margin-top "10px" :color "#dc3545"}}
            (str "Error: " error)))
       
       (when last-response
         ($ :div {:style {:margin-top "10px" :color "#28a745"}}
            ($ :div {:style {:display "flex" :align-items "center" :gap "10px"}}
               ($ :span "✓ Data fetched successfully!")
               ($ :button
                  {:onClick #(rf/dispatch [:planio/convert-to-pert-tasks])
                   :style {:padding "4px 8px"
                           :background "#28a745"
                           :color "white"
                           :border "none"
                           :border-radius "4px"
                           :cursor "pointer"
                           :font-size "12px"}}
                  "Import to PERT"))
            ($ :details
               ($ :summary {:style {:cursor "pointer"}} "View raw response")
               ($ :pre {:style {:background "#f8f9fa" :padding "10px" :overflow "auto" :max-height "200px"}}
                  (str last-response))))))))