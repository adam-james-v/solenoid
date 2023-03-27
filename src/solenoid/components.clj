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

(def ^:private input-map-classes
  ["border-form-stroke" "text-body-color" "placeholder-body-color"
   "max-w-20"
   "focus:border-primary" "active:border-primary" "rounded-md" "border-[1px]" "outline-none" "transition"
   "disabled:cursor-default" "disabled:bg-[#F5F7FD]"])

(defn- make-input-map
  [{:keys [id value control-type]}]
  {:id              id
   :class           input-map-classes
   :type            (control-type->input-type control-type)
   :value           (str value)
   :hx-get          (str "/controller/" id)
   :hx-trigger      "input"})

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
                 value-container-map-overrides]}              (merge base-component-default-opts opts)
         form-key                                             (control-type->form-key control-type)]
     [:div.grid.gap-1.self-center.text-xs
      {:hx-swap-oob "morphdom"
       :hx-swap     "none"
       :id          (str (name id) "-container")
       :style       {:align-items           "center"
                     :justify-items         "left"
                     :grid-template-columns "minmax(0, 1fr) minmax(0, 1fr) minmax(0, 5fr) minmax(0, 1fr) minmax(0, 1fr)"}}
      [:span.font-bold (str (or display-name id))]
      (when min [:span min])
      [form-key
       (merge
         (make-input-map control)
         {:hx-vals (make-hx-vals control get-value-fn)}
         control
         input-map-overrides)
       ;; if there's a form like a textarea, we need to insert the value within that tag so it shows up
       (when (#{:textarea} form-key) (str value))]
      (when max [:span max])
      [:span
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

;; this can be improved. It's halfway between only working on a single point [2 3]
;; and a list of points... Figure out how to make this more general/useful.
;; maybe all 2D drawing type controls should be in their own namespace.
;; also, it might be cool to write the js side of this with squint cljs?
(defn render-point-value [control [x y :as v]]
  (let [id (str (name (:id control)) "-pt")]
    [:g
     {:id id}
     [:svg [:circle.draggable
            {:fill       "lightgreen" :r 5 :cx 0 :cy 0
             :transform  (format "translate(%s,%s)" x y)
             :hx-trigger "drag"
             :hx-get     (str "/controller/" (:id control))
             :hx-swap    "none"
             :hx-vals    (make-hx-vals control (constantly "[pos.x, pos.y]"))}]]
     [:text
      {:fill      "gray"
       :transform (format "translate(%s,%s)" (+ x 7.5) (+ y 3.5))}
      (str v)]]))

(defmethod render-controller Point [control]
  (let [pts                       (into {} (map (fn [[x y]] [(gensym "pt") {:x x :y y}]) [(:value control)]))
        {:keys [id display-name]} control]
    [:div
     {:id id
      :hx-swap-oob "morphdom"}
     [:span (str (or display-name id))]
     (into
       [:svg {:width 150 :height 150
              :style {:background    "rgba(255,255,255,0.2)"
                      :touch-action  "none"
                      :cursor        "crosshair"
                      :border-radius "5px"} }
        #_[:svg
           (-> (svg-clj.elements/polygon (map (fn [[_ {:keys [x y]}]] [x y]) @asdf-pts))
               (svg-clj.transforms/style {:stroke "black"
                                          :fill   "none"}))]]
       (map (fn [[id {:keys [x y]}]]
              (render-point-value control [x y]))
            pts))]))

(defmulti render-control-block-result
  (fn [control-block]
    (-> control-block :state deref meta :result-type)))

(defmulti render-control-block
  (fn [control-block]
    (-> control-block :state deref meta :control-block-type)))

(defn render-control-block-result*
  "Base implementation for result render methods, useful for creating custom `render-control-block-result` methods."
  [{:keys [id]} result]
  [:div
   {:id          (str (name id) "-result")
    :class       ["control-block-result"]
    :hx-swap-oob "morphdom"}
   (or result "no result")])

(defmethod render-control-block-result :default
  [{:keys [state] :as control-block}]
  (let [result @state]
    (render-control-block-result* control-block result)))

(def ^:private button-group-classes
  ["bg-white/30" "rounded-lg" "hover:bg-gray-100" "duration-300" "transition-colors" "border" "px-3" "py-1" "text-xs"])

(defn render-control-block*
  "Base implementation for control block render methods, useful for creating custom `render-control-block` methods."
  [{:keys [id control-ids grid-w grid-h]} rendered-result]
  (let [controls (map @c/registry control-ids)]
    [:div.text-gray-600.shadow-md.hover:shadow-2xl.rounded-lg.text-sm.grid
     {:id          id
      :hx-swap-oob "true"
      :hx-swap     "outerHTML"
      :class       ["bg-white/30" "hover:bg-white/50" "backdrop-blur-md"]
      :style       {:grid-template-rows "min-content min-content 1fr min-content"
                    :grid-area          (format "span %s / span %s" (or grid-h 3) (or grid-w 4))}}
     [:input {:type "hidden" :name "item" :value (name id)}]
     ;; top bar
     [:div.grid.grid-cols-2.rounded-t-lg.p-1.border-b.border-slate-400.drag-handle
      [:h5.pl-2 id]
      [:button.place-self-end
       {:hx-get  (str "/action/delete/" id)
        :hx-swap "none"}
       [:svg {:width 15 :height 15} [:circle {:r 5 :cx 5 :cy 5 :fill "#F43F5E"}]]]]
     ;; controls
     (when (seq controls)
       (into [:div.grid.gap-1.p-1.border-b.border-slate-400.px-10.py-5.max-w-md] (mapv render-controller controls)))
     ;; result
     (when rendered-result
       [:div {:class ["control-block-result" "overflow-y-auto" "shadow-inner"]} rendered-result])
     ;; bottom bar/button group
     [:div.flex.items-center.gap-1.rounded-b-lg.p-1.control-block-bottom-bar
      (when rendered-result {:class ["border-t" "border-slate-400"]})
      [:button
       {:class   button-group-classes
        :hx-get  (str "/action/def/" id)
        :hx-swap "none"} "def"]
      [:button
       {:class   button-group-classes
        :hx-get  (str "/action/adjust-size/" id)
        :hx-vals (json/generate-string {:direction :width :op :-})
        :hx-swap "none"} "W-"]
      [:button
       {:class   button-group-classes
        :hx-get  (str "/action/adjust-size/" id)
        :hx-vals (json/generate-string {:direction :width :op :+})
        :hx-swap "none"} "W+"]
      [:button
       {:class   button-group-classes
        :hx-get  (str "/action/adjust-size/" id)
        :hx-vals (json/generate-string {:direction :height :op :-})
        :hx-swap "none"} "H-"]
      [:button
       {:class   button-group-classes
        :hx-get  (str "/action/adjust-size/" id)
        :hx-vals (json/generate-string {:direction :height :op :+})
        :hx-swap "none"} "H+"]]]))

(defmethod render-control-block :default
  [control-block]
  (render-control-block* control-block (render-control-block-result control-block)))

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
