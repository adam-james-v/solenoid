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
  "Creates an atom that updates when its binding references update."
  [bindings & formula]
  `(let ~bindings
     (let [formula#   (atom ~@formula)
           update-fn# (fn [key# ref# o# n#]
                        (let [attempt# (try ~@formula (catch Exception _e# nil))]
                          ;; the formula attempt must succeed, and the new value must not be nil
                          ;; this lets the formula 'recover' if any driving refs cause invalid state
                          (when (and (some? n#) (some? attempt#))
                            (swap! formula# (fn [_#] ~@formula)))))]
       (doseq [r# ~(vec (take-nth 2 bindings))]
         (add-watch r# ~(keyword (str "update-formula-" (gensym))) update-fn#))
       formula#)))
