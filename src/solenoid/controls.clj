(ns solenoid.controls
  (:require [solenoid.macros :as sm]
            [clojure.set :as set]
            [clojure.string :as str]
            [solenoid.utils :as u]))

(defonce event (atom :waiting :validator #(#{:waiting :reload} %)))
(defonce registry (atom {} :validator map?))

(defn registry-watcher
  [_ _ old new]
  ;; only perform actions when controllers are added or removed
  (when (not= (keys old) (keys new))
    ;; send a reload event to the frontend, so controllers changes are reflected in the UI
    (reset! event :reload)
    ;; when controllers are removed, remove any watches associated with affected cursor paths
    (let [removed (set/difference (set (keys old)) (set (keys new)))]
      (when (seq removed)
        (let [watches (u/get-watches registry)
              paths   (mapcat #(filter (fn [k]
                                         (and (vector? k)
                                              (= (first k) %))) watches) removed)]
          (when (seq paths)
            (doseq [path paths]
              (remove-watch registry path))))))))

(add-watch registry :change registry-watcher)

(defprotocol Control
  "Protocol for a Control Element."
  ;; every control needs a way to validate its own value:
  (validate [this value] "Method for validating the input to the control's value.")
  ;; every control needs a way to be rendered into some UI:
  (render [this] "Method for rendering the control into a UI."))

(defn- gen-id-key
  [obj]
  (let [prefix (-> (class obj)
                   str
                   (str/split #"\.")
                   last
                   str/lower-case)
        sym (gensym (str prefix "-"))]
    (keyword sym)))

(defn register!
  "Method for registering the control element into an atom."
  [{:keys [id] :as this}]
  (let [id (or id (gen-id-key this))
        control (assoc this :id id)]
    (when-not ((set (keys @registry)) id)
      (swap! registry assoc id control))
    control))

(defn add-type-key
  [control]
  (let [k (-> (type control)
              str
              (str/split #"\.")
              last
              str/lower-case
              keyword)]
    (assoc control :control-type k)))

(defn un-register
  "Method for un-registering the control element into an atom."
  [{:keys [id]}]
  (swap! registry dissoc id))

(defn change
  "Method for updating the control's value."
  [{:keys [id] :as this} value]
  (when (validate this value)
    (swap! registry assoc id (assoc this :value value))))

;; Controls to implement:
;; - toggle
;; - radio group
;; - dropdown
;; - multi-select (checkbox group)
;; - editable table (for maps)
;; - editable table (for vector of vectors)

(defrecord Num [control-type id value min max step display-name]
  Control
  (validate [_ value]
    (when (number? value) value)))

(defn make-num [{:keys [value] :as m}]
  (let [default-values {:control-type :num}]
    (map->Num (merge default-values m {:value (or value 1)}))))

(defrecord Slider [control-type id value min max step display-name]
  Control
  (validate [{:keys [min max]} value]
    (when (<= min value max) value)))

(defn make-slider [{:keys [value] :as m}]
  (let [default-values {:control-type :slider
                        :min 1
                        :max 100
                        :step 1}]
    (map->Slider (merge default-values m {:value (or value 1)}))))

(defrecord Text [control-type id value display-name]
  Control
  (validate [_ value]
    (when (string? value) value)))

(defn make-text [{:keys [value] :as m}]
  (let [default-values {:control-type :text}]
    (map->Text (merge default-values m {:value (or value "asdf")}))))

(defn- maybe-read-string
  [s]
  (try (read-string s)
       (catch Exception _e nil)))

(defrecord EdnBlock [control-type id value display-name]
  Control
  (validate [_ value]
    (maybe-read-string (str value))))

(defn make-edn-block [{:keys [value] :as m}]
  (let [default-values {:control-type :edn}]
    (map->EdnBlock (merge default-values m {:value (or value :default-value)}))))

(def control-key->control-fn
  {:num make-num
   :slider make-slider
   :text make-text
   :edn make-edn-block})

;; PROBLEM: should these implement the Control Protocol?
(defrecord Action [id text f])
(defrecord ControlBlock [id control-ids state])

(defn- make-cursor-bindings
  "Turn binding vector of symbols and controls records into binding of symbols cursors into the control value"
  [bindings]
  (vec (mapcat (fn [[sym {id :id}]] [sym `(sm/cursor registry [~id :value])]) (partition 2 bindings))))

(defn- make-deref-bindings
  [bindings]
  (vec (mapcat (fn [[sym _]] [sym `(deref ~sym)]) (partition 2 bindings))))

(defn infer-control
  "Look at the value and return a control"
  [value]
  (cond
    ;; if the value is already a control, we're good
    (satisfies? Control value) value

    (and (map? value)
         (or (contains? value :control-type)
             (contains? value :type)))
    ((control-key->control-fn (or (:type value) (:control-type value))) (dissoc value :type))

    (ratio? value)  (make-num {:value (double value)})
    (number? value) (make-num {:value value})
    (string? value) (make-text  {:value value})

    :else (make-edn-block {:value value})))

;; PROBLEM: remove the 'eval' in there??
;; PROBLEM: if you macroexpand-1 this, it still side-effects (adds your bindings into the registry)
;; I think there's a design problem here...

;; what is this macro doing?
;; 1. get the symbols used in the let-style binding passed in
;; 2. generate a unique control-block keyword
(defmacro letcontrols
  [bindings & body]
  (let [syms             (vec (take-nth 2 bindings))
        block-id#        (keyword (gensym "controlblock-"))
        ;; controls are registered here
        controls         (map (comp
                                register!
                                (fn [m] (assoc m :block-id block-id#))
                                infer-control
                                eval)
                              (take-nth 2 (rest bindings)))
        cursor-bindings# (make-cursor-bindings (interleave syms controls))
        deref-bindings#  (make-deref-bindings cursor-bindings#)]
    `(let ~bindings
       ;; create a formula with the agents linked to each control binding
       (-> (register!
             (map->ControlBlock {:id          ~block-id#
                                 :control-ids ~(mapv :id controls)
                                 :state       (sm/formula ~cursor-bindings#
                                                          (let ~deref-bindings#
                                                            ~@body))}))
           :state))))
