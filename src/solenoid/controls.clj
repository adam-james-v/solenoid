(ns solenoid.controls
  (:require [solenoid.macros :as sm]
            [clojure.set :as set]
            [clojure.string :as str]
            [solenoid.utils :as u]))

(defonce registry (atom {} :validator map?))

(defn registry-watcher
  [_ _ old new]
  ;; only perform actions when controllers are added or removed
  (when (not= (keys old) (keys new))
    ;; when controllers are removed, remove any watches associated with affected cursor paths
    (let [removed (set/difference (set (keys old)) (set (keys new)))]
      (when (seq removed)
        (let [cb-watches (->> (keys new)
                              (filter #(str/starts-with? (name %) "controlblock"))
                              (mapcat #(u/get-watches (get-in new [% :state])))
                              (filter #(removed (last %))))
              watches    (u/get-watches registry)
              paths      (mapcat #(filter (fn [k]
                                            (and (vector? k)
                                                 (= (first k) %))) watches) removed)]
          ;; any removed control blocks might have been dependents of a still-existing block.
          ;; In such cases, old watches on still-existing blocks must be removed so that we don't get any NPEs
          (when (seq cb-watches)
            (doseq [cb (keys new)
                    w  cb-watches]
              (swap! registry update-in [cb :state] (fn [ref]
                                                      (when ref (remove-watch ref w))))))
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

(defrecord Toggle [control-type id value display-name]
  Control
  (validate [_ value]
    (when (true? value) false)))

(defn make-toggle [{:keys [value] :as m}]
  (let [default-values {:control-type :toggle}]
    (map->Toggle (merge default-values m {:value (boolean value)}))))

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

(defrecord Point [control-type id value display-name]
  Control
  (validate [_ value]
    (maybe-read-string (str value))))

(defn make-point [{:keys [value] :as m}]
  (let [default-values {:control-type :point}]
    (map->Point (merge default-values m {:value (or value [0 0])}))))

(def control-key->control-fn
  {:num    make-num
   :slider make-slider
   :toggle make-toggle
   :text   make-text
   :point  make-point
   :edn    make-edn-block})

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

    (ratio? value)               (make-num {:value (double value)})
    (number? value)              (make-num {:value value})
    (string? value)              (make-text  {:value value})
    (boolean? value)             (make-toggle {:value value})
    (and (vector? value)
         (= (count value) 2)
         (every? number? value)) (make-point {:value value})

    :else (make-edn-block {:value value})))

(defn- get-derefs
  [form]
  (->> form
       (tree-seq seqable? identity)
       (filter #(and (not (map? %)) (seqable? %)))
       (map (fn [[sym r]]
              (when (or (= 'deref sym)
                        (= 'clojure.core/deref sym))
                r)))
       (remove nil?)
       distinct
       vec))

;; PROBLEM: remove the 'eval' in there??
;; PROBLEM: if you macroexpand-1 this, it still side-effects (adds your bindings into the registry)

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
        ;; Get any derefs that are within the body of the letcontrols
        derefs-in-body#  (get-derefs body)
        ;; Add these derefs so that watches are properly registsered to all refs that drive this block.
        cursor-bindings# (vec (concat
                                (make-cursor-bindings (interleave syms controls))
                                (interleave (map #(-> % name symbol) derefs-in-body#) derefs-in-body#)))
        ;; `make-deref-bindings` builds up the let binding vector
        ;; for the regular let that sits inside the formula macro.
        ;; That let form is needed for re-binding the user's symbols to the cursors for each control.
        ;; we remove the derefs-in-body because those will have been de-referenced in the body already.
        deref-bindings#  (make-deref-bindings (remove (set derefs-in-body#) cursor-bindings#))
        ;; cursor-bindings will add the optional :dependents key with this block's ID
        ;; so that watches can be added properly to all refs used in the body. See `solenoid.macros/formula`.
        cursor-bindings# (vec (concat
                                cursor-bindings#
                                (when (seq derefs-in-body#) [:dependents block-id#])))]
    `(let ~bindings
       ;; create a formula with the atoms linked to each control binding
       (-> (register!
             (map->ControlBlock
               {:id          ~block-id#
                :control-ids ~(mapv :id controls)
                :state       (let [state# (sm/formula ~cursor-bindings#
                                                      (let ~deref-bindings#
                                                        ~@body))]
                               state#)}))
           :state))))

(defmacro letview
  [& body]
  (let [block-id#        (keyword (gensym "viewblock-"))
        ;; Get any derefs that are within the body of the letcontrols
        derefs-in-body#  (get-derefs body)
        ;; Add these derefs so that watches are properly registsered to all refs that drive this block.
        cursor-bindings# (vec (concat ['_ nil] (interleave (map #(-> % name symbol) derefs-in-body#) derefs-in-body#)))
        ;; `make-deref-bindings` builds up the let binding vector
        ;; for the regular let that sits inside the formula macro.
        ;; That let form is needed for re-binding the user's symbols to the cursors for each control.
        ;; we remove the derefs-in-body because those will have been de-referenced in the body already.
        deref-bindings#  ['_ nil]
        ;; cursor-bindings will add the optional :dependents key with this block's ID
        ;; so that watches can be added properly to all refs used in the body. See `solenoid.macros/formula`.
        cursor-bindings# (vec (concat
                                cursor-bindings#
                                (when (seq derefs-in-body#) [:dependents block-id#])))]
    `(-> (register!
           (map->ControlBlock
             {:id          ~block-id#
              :state       (let [state# (sm/formula ~cursor-bindings#
                                                    (let ~deref-bindings#
                                                      ~@body))]
                             state#)}))
         :state)))
