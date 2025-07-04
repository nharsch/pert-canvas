(ns pert-canvas.ui.state)

(def state-task
  [:map
   [:id :int]
   [:label :string]
   [:dependencies [:set :int]]])


(def initial-tasks
  [
   {:id 1
    :label "Buy Ingredients"
    :description "Buy Ingredients"}
   {:id 2
    :label "Mix Ingredients"
    :description "Mix Ingredients"
    :dependencies #{1}}
   {:id 3
    :label "place dough on pan"
    :description ""
    :dependencies #{2}}
   {:id 4
    :label "Bake dough"
    :description ""
    :dependencies #{3 5}}
   {:id 5
    :label "Preheat Oven"
    :description ""
    :dependencies #{}}
   {:id 6
    :label "Eat Cookies"
    :description ""
    :dependencies #{4}}
   ])

(def initial-state
  {
   :app/tasks initial-tasks
   :app/selected-task nil
   :app/selected-edge nil
   })
