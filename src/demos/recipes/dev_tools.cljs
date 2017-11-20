(ns recipes.dev-tools
  (:require
    [om.dom :as dom]
    [om.next :as om :refer [defui]]
    [fulcro.client.routing :as r]
    [fulcro.client.mutations :as m]
    [fulcro.client.core :as fc :refer [InitialAppState initial-state]]
    [fulcro.client.data-fetch :as df]
    [goog.object :as obj]))

(defn target-app []
  (obj/get js/window "app"))

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

(defui ^:once RootKeysTab
  static InitialAppState
  (initial-state [clz params] {:kind :root-keys})
  static om/IQuery
  (query [this] [:kind])
  Object
  (render [this]
    (let [reconciler       (-> @(target-app) :reconciler)
          reconciler-state @(:state reconciler)]
      (dom/div nil
        (str (root-query-keys reconciler-state))))))

(defui ^:once TablesTab
  static InitialAppState
  (initial-state [clz params] {:kind :tables})
  static om/IQuery
  (query [this] [:kind])
  Object
  (render [this]
    (let [reconciler (-> @(target-app) :reconciler)
          app-state  (-> reconciler :config :state)]
      (dom/div nil
        (str (table-names @app-state))))))

(defn ident-like? [i]
  (and (vector? i)
       (= 2 (count i))))

(m/defmutation search-by-ident [{:keys [text]}]
  (action [{:keys [state]}]
    (let [target-ident (cljs.reader/read-string text)]
      (swap! state assoc-in [:lookup :singleton :target-ident]
             target-ident))))

(defui ^:once Lookup
  static InitialAppState
  (initial-state [clz params] {:target-ident nil
                               :text         ""})
  static om/IQuery
  (query [this] [:target-ident :text])
  static om/Ident
  (ident [this props] [:lookup :singleton])
  Object
  (render [this]
    (let [reconciler (-> @(target-app) :reconciler)
          app-state  (-> reconciler :config :state)

          {:keys [target-ident text]} (om/props this)]
      (dom/div nil
        (dom/div nil
          (dom/input #js {:type     "text"
                          :value    text
                          :onChange #(m/set-string! this :text :event %)})
          (dom/button #js {:onClick #(om/transact! this `[(search-by-ident {:text ~text})])}
            "Go"))
        (when (ident-like? target-ident)
          (dom/div nil
            (str (ident->data @app-state target-ident))))))))

(def ui-lookup (om/factory Lookup))

(defui ^:once LookupTab
  static InitialAppState
  (initial-state [clz params] {:kind   :lookup
                               :lookup (initial-state Lookup nil)})
  static om/IQuery
  (query [this] [:kind {:lookup (om/get-query Lookup)}])
  Object
  (render [this]
    (let [{:keys [lookup]} (om/props this)]
      (ui-lookup lookup))))


(r/defrouter UITabs :ui-router
  (ident [this {:keys [kind]}] [kind :tab])
  :root-keys RootKeysTab
  :tables TablesTab
  :lookup LookupTab)

(def ui-tabs (om/factory UITabs))

(m/defmutation choose-tab [{:keys [tab]}]
  (action [{:keys [state]}]
    (swap! state r/set-route :ui-router [tab :tab])))

(defui ^:once Root
  static InitialAppState
  (initial-state [clz params] {:ui/react-key "initial"
                               :current-tab  (initial-state UITabs nil)})
  static om/IQuery
  (query [this] [:ui/react-key {:current-tab (om/get-query UITabs)}])
  Object
  (render [this]
    (let [{:keys [ui/react-key current-tab] :as props} (om/props this)

          tab-select #(om/transact! this `[(choose-tab {:tab ~%})])]
      (dom/div #js {:key react-key}
        (dom/div nil
          (dom/button #js {:onClick (partial tab-select :root-keys)}
            "Root Keys")
          (dom/button #js {:onClick (partial tab-select :tables)}
            "Tables")
          (dom/button #js {:onClick (partial tab-select :lookup)}
            "Lookup"))
        (ui-tabs current-tab)))))




