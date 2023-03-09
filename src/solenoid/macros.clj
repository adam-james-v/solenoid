(ns solenoid.macros)

;; inspiration from `defformula` pg 304 of The Joy of Clojure (337 of the pdf)
(defmacro cursor
  "Creates an atom that is updated to the value in the `reference` found at the given `path`."
  [reference path]
  (let [watch-key (conj path (keyword (gensym)))]
    `(let [val#       (atom (get-in (deref ~reference) ~path))
           update-fn# (fn [key# ref# o# n#]
                        (swap! val# (fn [_#] (get-in (deref ~reference) ~path))))]
       (add-watch ~reference ~watch-key update-fn#)
       val#)))

(defmacro formula
  "Creates an atom that updates when its binding references update.
  Optionally, the last binding can be a keyword `:dependents` with a keyword value.
  This dependents keyword is used to construct the add-watch key which will have the form:
  `[:update-formula-11111 :controlblock-11111]`.

  This is done so that you can find all controlblocks that are dependent on this formula
  by using `solenoid.utils/get-watches` and looking at the last element in each result."
  [bindings & formula]
  (let [l2 (take-last 2 bindings)
        dependents (when (= (first l2) :dependents) (second l2))
        bindings (if dependents (vec (drop-last 2 bindings)) bindings)]
    `(let ~bindings
       (let [formula#   (atom ~@formula)
             update-fn# (fn [key# ref# o# n#]
                          (let [attempt# (try ~@formula (catch Exception _e# nil))]
                            ;; the formula attempt must succeed, and the new value must not be nil
                            ;; this lets the formula 'recover' if any driving refs cause invalid state
                            (when (and (some? n#) (some? attempt#))
                              (swap! formula# (fn [_#] ~@formula)))))]
         (doseq [r# ~(vec (take-nth 2 bindings))]
           ;; adds a watch to each atom in the binding that is responsible for
           ;; updating this formula whenever that atom changes.
           (when r#
             (add-watch r# ~(vec (remove nil? [(keyword (gensym "update-formula-")) dependents])) update-fn#)))
         formula#))))
