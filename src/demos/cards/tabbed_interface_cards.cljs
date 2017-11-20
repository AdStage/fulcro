(ns cards.tabbed-interface-cards
  (:require
    [devcards.core :as dc :include-macros true]
    [recipes.tabbed-interface-client :as client]
    [fulcro.client.cards :refer [defcard-fulcro]]
    [om.dom :as dom]
    [fulcro.client.data-fetch :as df]
    [fulcro.client.logging :as log]
    [fulcro.client.core :as fc]
    [recipes.dev-tools :as dev-tools]
    [om.next :as om]
    [goog.object :as obj]))

(defcard-fulcro tabbed-card
  "
  # Tabbed Interface with Pane Content Dynamic Loading

  Note: This is a full-stack example. Make sure you're running the server and are serving this page from it.
  "
  client/Root
  {}
  {:inspect-data true})

(obj/set js/window "app" tabbed-card-fulcro-app)

(defcard-fulcro dev-tools
  dev-tools/Root
  {}
  {:inspect-data true})


(comment
  (let [reconciler       (-> @tabbed-card-fulcro-app :reconciler)
        app-state        (-> reconciler :config :state)
        reconciler-state @(:state reconciler)]
    (root-query-keys reconciler-state)
    )

  )

(defn root-query-keys [reconciler-state]
  (let [root-comp (:root reconciler-state)]
    (map (fn [q-elem]
           (cond
             (map? q-elem) (-> q-elem keys first)
             :else         q-elem))
         (om/get-query root-comp))))

(defn tables-only [state]
  (->> state
       (filter (fn [[_ v]]
                 (and (map? v) (every? map? (vals v)))))
       (into {})))

(defn table-names [state]
  (let [tables (tables-only state)]
    (map (fn [[k]] (str k)) tables)))

(defn ident->data [state ident]
  (get-in state ident))


