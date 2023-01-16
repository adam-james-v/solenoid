(ns solenoid.components
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [solenoid.utils :as u]
            [solenoid.controls :as c])
  (:import [solenoid.controls Slider Num Text EdnBlock]))

(defn action-button
  [{:keys [id text]}]
  [:button.btn-close
   {:hx-post (str "/action/" id)
    :hx-swap "none"}
   (or text id)])

;; PROBLEM: make the components take in the exact shape of the record, nothing more
;; PROBLEM: make the components return the exact shape of the record, with updated value
;;          do this by having some to/from json fns that jsonify the control record coming in,
;;          and a fn that properly builds the hx-vals 'map'
;; PROBLEM: de-dupe the structure of the controls renderers a bit (eg. controls-key/id/class controls-val-key/id/class etc.)


(defn- make-hx-vals
  [m get-value-js-str]
  (-> (str "js:" (json/generate-string (assoc m :value "____")))
      (str/replace #"\"____\"" get-value-js-str)))

(def control-type->input-type
  {:slider "range"
   :num    "number"
   :text   "text"
   :edn    "text"})

(defn- make-input-map
  [{:keys [id value control-type]}]
  {:id         id
   :class      ["form-control-sm" (name control-type)]
   :type       (control-type->input-type control-type)
   :value      (str value)
   :hx-get     (str "/controller/" id)
   :hx-trigger "input"
   :hx-target  (str "#" (name id) "-value")})

(defn base-component
  ([control]
   (base-component
     control {} {}))

  ([{:keys [id display-name value min max control-type] :as control} input-map-overrides value-container-map-overrides]
   (let [get-value-js-str (str "document.getElementById('" (name id) "').value")]
     [:div.control.row
      [:span.col-4 (str (or display-name id))]
      (when min [:span.col min])
      [:span.col {:class (str (name control-type) "-container")}
       [:input
        (merge
          (make-input-map control)
          {:hx-vals (make-hx-vals control get-value-js-str)}
          control
          input-map-overrides)]]
      (when max [:span.col max])
      [:span.col-1
       (merge
         {:id    (str (name id) "-value")
          :style {:display "none"}}
         value-container-map-overrides) (str value)]])))

#_(defn slider
  [{:keys [id min max value block-id] :as val-map}]
  [:div.control.row
   [:span.col-4 (str id "  ")]
   [:span.col-1 min]
   [:span.slider-container.col
    [(keyword (str "input#" (name id) ".slider"))
     (merge
       {:type       "range"
        :min        1
        :max        100
        :val        50
        :hx-get     (str "/controller/" id)
        :hx-target  (str "#" (name id) "-value")
        :hx-trigger "input"
        :hx-vals    (str "js:{"
                         "\"block-id\": \"" block-id "\", "
                         "type: \":slider\", "
                         "value: document.getElementById('" (name id) "').value, "
                         "min: " min ", "
                         "max: " max ", " "}")}
       val-map)]]
   [:span.col-1 max]
   [(keyword (str "div#" (name id) "-value" ".col-1"))
    {:style {:display     "inline-block"
             :margin-left "10px"}}
    value]])

#_(defn number
  [{:keys [id value block-id] :as val-map}]
  [:div.control.row
   [:span.col-4 (str id "  ")]
   [:span.num-container.col-2
    [(keyword (str "input#" (name id) ".number" ".form-control-sm"))
     (merge
       {:type      "number"
        :min       1
        :hx-get    (str "/controller/" id)
        :hx-target (str "#" (name id) "-value")
        :hx-vals   (str "js:{"
                         "\"block-id\": \"" block-id "\", "
                         "type: \":num\", "
                         "value: document.getElementById('" (name id) "').value, "
                         #_#_#_"min: " min ", "
                         #_#_#_"max: " max ", " "}")}
       val-map)]
    [(keyword (str "div#" (name id) "-value"))
     {:style {:display "none"}}
     value]]])

#_(defn text
  [{:keys [id value] :as val-map}]
  [:div.control.row
   [:span.col-4 (str id "  ")]
   [:span.text-container.col-3
    [(keyword (str "input#" (name id) ".text" ".form-control-sm"))
     (merge
       {:type      "text"
        :hx-get    (str "/controller/" id)
        :hx-swap   "none"
        :hx-vals   (str "js:{"
                        "type: \":text\", "
                        "value: document.getElementById('" (name id) "').value, "
                        "}")}
       val-map)]]
   [(keyword (str "div#" (name id) "-value"))
    {:style {:display "none"}}
    value]])

(defn edn-block
  [{:keys [id value block-id]}]
  [:div.control.row
   [:span.col-4 (str id "  ")]
   [:span.edn-container
    [(keyword (str "input#" (name id) ".edn" ".form-control"))
     {:type    "text"
      :value   (str value)
      :hx-get  (str "/controller/" id)
      :hx-swap "none"
      :hx-vals (str "js:{"
                    "\"block-id\": \"" block-id "\", "
                    "control-type: \":edn\", "
                    "value: document.getElementById('" (name id) "').value, "
                    "}")}]]])

#_(defn toggle
  [{:keys [id value] :as val-map}]
  [:div
   [:span (str id "  ")]
   [:span.togglecontainer
    [(keyword (str "input#" (name id) ".toggle"))
     (merge
       {:type      "toggle"
        :checked   (u/maybe-parse-boolean value)
        :hx-get    (str "/controller/" id)
        :hx-target (str "#" (name id) "-value")
        :hx-vals   (str "js:{"
                        "value: document.getElementById('" (name id) "').checked, "
                        "type: 'toggle', " "}")}
       val-map)]
    [(keyword (str "div#" (name id) "-value"))
     {:style {:display "none"}}
     value]]])


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

(defmulti render-controller type)

(defmethod render-controller :default [m] (base-component m))
(defmethod render-controller Num      [m] (base-component m))
(defmethod render-controller Text     [m] (base-component m))
(defmethod render-controller Slider   [m] (base-component m {} {:style {:display "inline-block"}}))
(defmethod render-controller EdnBlock [m] (let [val (str (:value m))] (base-component (assoc m :value val))))
#_(defmethod render-controller "radio"    [m] (radio m))
#_(defmethod render-controller "toggle"   [m] (toggle m))
#_(defmethod render-controller "checkbox" [m] (checkbox m))
#_(defmethod render-controller "dropdown" [m] (dropdown m))


(defn render-control-block-result
  [{:keys [id state]} oob?]
  (let [result @state]
    [:div.text-center
     (merge
       {:id          (str (name id) "-result")
        :class       "control-block-result"}
       (when oob? {:hx-swap-oob "innerHTML"}))
     result]))

(defn render-control-block
  [{:keys [id control-ids] :as control-block}]
  (let [controls (map @c/registry control-ids)]
    [:div.col.g-4
     [:div.card.shadow {:id id}
      [:div.card-body
       [:div.row
        [:div.col [:h5.card-title id]]
        [:div.col-1
         [:button.btn-close.text-end
          {:hx-post (str "/action/delete/" id)
           :hx-swap "none"}]]]
       (into [:div.row.controls] (mapv render-controller controls))
       [:div.row (render-control-block-result control-block false)]
       [:div.row
        [:div.col [:button.btn.btn-outline-secondary.btn-sm
                   {:hx-post (str "/action/def/" id)
                    :hx-swap "none"} "def"]]]]]]))
