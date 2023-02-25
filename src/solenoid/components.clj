(ns solenoid.components
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [solenoid.controls :as c])
  (:import [solenoid.controls Slider Num Toggle Text EdnBlock Point]))

(defn action-button
  [{:keys [id text]}]
  [:button.btn-close
   {:hx-post (str "/action/" id)
    :hx-swap "none"}
   (or text id)])

(defn- make-hx-vals
  [m get-value-fn]
  (-> (str "js:" (json/generate-string (assoc m :value "____")))
      (str/replace #"\"____\"" (get-value-fn m))))

(def ^:private control-type->input-type
  {:slider "range"
   :num    "number"
   :text   "text"
   :toggle "checkbox"})

(def ^:private control-type->form-key
  {:slider :input
   :num    :input
   :text   :input
   :toggle :input
   :edn    :textarea
   :point  :div})

(defn- make-input-map
  [{:keys [id value control-type]}]
  {:id         id
   :class      ["form-control-sm" (name control-type) "col"]
   :type       (control-type->input-type control-type)
   :value      (str value)
   :hx-get     (str "/controller/" id)
   :hx-trigger "input"
   :hx-target  (str "#" (name id) "-value")})

(def ^:private base-component-default-opts
  {:input-map-overrides           {}
   :value-container-map-overrides {}
   :get-value-fn
   (fn [control]
     (str "document.getElementById('" (name (:id control)) "').value"))})

(defn base-component
  ([control]
   (base-component control base-component-default-opts))
  ([control opts]
   (let [{:keys [id display-name value min max control-type]} control
         {:keys [get-value-fn
                 input-map-overrides
                 value-container-map-overrides]} (merge base-component-default-opts opts)
         form-key (control-type->form-key control-type)]
     (intern 'user 'hx-vals (make-hx-vals control get-value-fn))
     [:div.control.row.my-1.mx-0.p-0
      [:span.col-3.text-end.mb-0.mt-2 (str (or display-name id))]
      [:span.col-7 {:class (str (name control-type) "-container")}
       [:span.row.small
        (when min [:span.col-2.text-end.mb-0.mt-1 min])
        [form-key
         (merge
           (make-input-map control)
           {:hx-vals (make-hx-vals control get-value-fn)}
           control
           input-map-overrides)
         ;; if there's a form like a textarea, we need to insert the value within that tag so it shows up
         (when (#{:textarea} form-key) (str value))]
        (when max [:span.col-2.mb-0.mt-1 max])]]
      [:span.col-2.mb-0.mt-1
       (merge
         {:id    (str (name id) "-value")
          :style {:display "none"}}
         value-container-map-overrides) value]])))

(defmulti  render-controller type)
(defmethod render-controller :default [m] (base-component m))
(defmethod render-controller Num      [m] (base-component m))
(defmethod render-controller Text     [m] (base-component m))
(defmethod render-controller Toggle   [m]
  (let [val (:value m)]
    (base-component m {:input-map-overrides {:checked val}
                       :get-value-fn (fn [control]
                                       (str "document.getElementById('" (name (:id control)) "').checked"))})))

(defmethod render-controller Slider [m]
  (base-component m {:value-container-map-overrides
                     {:style {:display "inline-block"}}}))

(defmethod render-controller EdnBlock [m]
  (let [val (str (:value m))]
    (base-component (assoc m :value val))))

(defn render-point-value
  [m]
  [:div {:style {:position "relative"
                 :display  "flex"
                 :left     (str (- (first (:value m)) 2) "px")
                 :top      (str (- (second (:value m)) 2) "px")
                 :margin-top "-2px"}}
   [:div {:style {:width "4px" :height "4px" :border-radius "4px" :background "red"}}]
   [:div {:style {:margin-top  "-0.45em !important"
                  :margin-left "0.2em"
                  :width       "auto"}} (str (:value m))]])

(defmethod render-controller Point [control]
  (let [val (render-point-value control)
        gvf (fn [control]
              (str
                "["
                "window.event.clientX" " - "
                "document.getElementById('" (name (:id control)) "').getBoundingClientRect().left" "\n,"
                "window.event.clientY" " - "
                "document.getElementById('" (name (:id control)) "').getBoundingClientRect().top"
                "]"))
        imo {:class      []
             :hx-trigger :click
             :style      {:width         "100px"
                          :height        "100px"
                          :padding       0
                          :border-radius "4px"
                          :background    "rgba(255,255,255,0.2)"
                          :cursor        "crosshair"}}

        {:keys [id display-name control-type]} control
        form-key                               (control-type->form-key control-type)]
     [:div.control.row.my-1.mx-0.p-0
      [:span.col-3.text-end.mb-0.mt-2 (str (or display-name id))]
      [:span.col-7 {:class (str (name control-type) "-container")}
       [:span.row.small
        [form-key
         (merge
           (make-input-map control)
           {:hx-vals (make-hx-vals control gvf)}
           control
           imo)
         [:span.col-2.mb-0.mt-1
          {:id    (str (name id) "-value")
           :style {:display  "block"
                   :class    nil
                   :position "absolute"}} val]]]]]))

(defmulti render-control-block-result
  (fn [control-block _]
    (-> control-block :state deref meta :result-type)))

(defmulti render-control-block
  (fn [control-block]
    (-> control-block :state deref meta :control-block-type)))

(defmethod render-control-block-result :default
  [{:keys [id state]} oob?]
  (let [result @state]
    [:div.text-center
     (merge
       {:id          (str (name id) "-result")
        :class       "control-block-result"}
       (when oob? {:hx-swap-oob "innerHTML"}))
     (or result "no result")]))

(defn render-control-block*
  "Base implementation for control block render methods, useful for creating custom `render-control-block` methods."
  [{:keys [id control-ids]} rendered-result]
  (let [controls (map @c/registry control-ids)]
    [:div.col.g-4
     [:div.card {:id id}
      [:div.card-header
       [:span.row
        [:h5.col.card-title.mb-0.mt-1 id]
        [:button.btn-close.text-end.col-1.px-2
         {:hx-post (str "/action/delete/" id)
          :hx-swap "none"}]]]
      [:div.card-body
       (into [:div.row.controls] (mapv render-controller controls))
       [:div.row.my-3 rendered-result]
       [:div.row
        [:div.col [:button.btn.btn-outline-secondary.btn-sm
                   {:hx-post (str "/action/def/" id)
                    :hx-swap "none"} "def"]]]]]]))

(defmethod render-control-block :default
  [control-block]
  (render-control-block*
    control-block
    (render-control-block-result control-block false)))

#_(defn dropdown
  [{:keys [id value options] :as val-map}]
  (let [select [(keyword (str "select#" (name id) ".dropdown"))
                (merge
                  {:hx-get     (str "/controller/" id)
                   :hx-target  (str "#" (name id) "-value")
                   :hx-vals    (str "js:{"
                                    "value: document.getElementById('" (name id) "').value, "
                           "type: 'dropdown', " "}")}
                  val-map)]]
    [:div
     [:span (str id "  ")]
     [:span.dropdowncontainer
      (into select (for [option options]
                     [:option.dropdown-option {:value option} option]))
      [(keyword (str "div#" (name id) "-value"))
       {:style {:display     "none"}}
       value]]]))

#_(defn radio
  [{:keys [id value options] :as val-map}]
  [:div
   [:span (str id "  ")]
   [:span.radiocontainer
    (into [:span]
          (for [[idx option] (map vector (range) options)]
            (let [option-id (str (name id) "-" idx)]
              [:span
               [(keyword (str "input#" option-id ".radio-option"))
                {:type      "radio"
                 :name      id
                 :value     option
                 :hx-get    (str "/controller/" id)
                 :hx-target (str "#" (name id) "-value")
                 :hx-vals   (str "js:{"
                                 "value: document.getElementById('" option-id "').value, "
                                 "type: 'radio', " "}")}]
               [:label {:for option-id} option]])))
    [(keyword (str "div#" (name id) "-value"))
     {:style {:display "none"}}
     value]]])

#_(defn checkbox
  [{:keys [id value] :as val-map}]
  [:div
   [:span (str id "  ")]
   [:span.checkboxcontainer
    (into [:span]
          (for [[idx [option v]] (map vector (range) value)]
            (let [option-id (str (name id) "-" idx)
                  new-value (-> value
                                (assoc option (str "document.getElementById('" option-id "').checked, "))
                                (update-keys #(str (name %) ":")))]
              [:span
               [(keyword (str "input#" option-id ".checkbox-option"))
                {:type      "checkbox"
                 :name      id
                 :value     option
                 :checked   v
                 :hx-get    (str "/controller/" id)
                 :hx-target (str "#" (name id) "-value")
                 :hx-vals   (str "js:{"
                                 "value: document.getElementById('" option-id "').checked, "
                                 "option: " option-id ", "
                                 "type: 'checkbox', " "}")}]
               [:label {:for option-id} option]])))
    [(keyword (str "div#" (name id) "-value"))
     {:style {:display "none"}}
     value]]])

;; PROBLEM
;; It might be good to REDESIGN:
;; controls need a re-design I think. Each control added needs:

;; - define a record in controls
;; - define a validator in the record in controls
;; - define a maker-fn in controls
;; - define a conditional for inferring the controls from some input in controls
;; - add entry in control-key->control-fn map in controls

;; - import the control class into components
;; - add entry to control-type->form-key
;; - add entry to control-type->input-type IF using :input
;; - define a render-controller method in components

;; - know if your render-controller needs a render-controller-value fn
;; - add a case for control-type keys in the controller/:id server response IF special case is needed
