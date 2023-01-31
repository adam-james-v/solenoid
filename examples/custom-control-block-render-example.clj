(ns custom-control-block-render-example
  (:require [solenoid.controls :as c]
            [solenoid.components :as components]))

(comment
  ;; What can you do if you want to render your data in a nice way?
  ;; You can always create whatever renderable structure you want inside the letcontrol body:

  (c/letcontrols
    [my-table-data [{:A 1 :B 2 :C 3}
                    {:A 2 :B 2 :C 3}
                    {:A 3 :B 2 :C 3}
                    {:A 20 :B 2 :C 3}]]
    (let [ks     (distinct (mapcat keys my-table-data))
          header (into [:tr] (map (fn [h] [:th (name h)])) ks)]
      [:span
       [:h4 "My Table"]
       (into
         [:table.table header]
         (for [row my-table-data]
           (into [:tr] (map (fn [k] [:td (or (get row k) "")]) ks))))]))

  ;; Well, that's nice, but nothing new.
  ;; What if you have a lot of similarly shaped data?
  ;; It would be nice to pull the rendering out in a function, for reuse:

  (defn make-table
    ([row-maps] (make-table row-maps nil))
    ([row-maps title]
     (let [ks     (distinct (mapcat keys row-maps))
           header (into [:tr] (map (fn [h] [:th (name h)])) ks)]
       [:span
        (when title [:h4 title])
        (into
          [:table.table header]
          (for [row row-maps]
            (into [:tr] (map (fn [k] [:td (or (get row k) "")]) ks))))])))

  (c/letcontrols
    [my-table-data [{:A 1 :B 2 :C 3}
                    {:A 2 :B 2 :C 3}
                    {:A 3 :B 2 :C 3}
                    {:A 20 :B 2 :C 3}]]
    (make-table my-table-data "My Table 2"))


  ;; That's pretty good, but still not actually anything new.
  ;; Suppose you actually wanted to alter your table data with the control block,
  ;; and deref it back into your repl as the same structure you set up in the binding.
  ;; With the above approach, you have reusability, but you don't have access to the
  ;; original table data shape anymore, you've transformed it into the renderable version.
  ;; What can you do instead?

  ;; Define a custom method to `solenoid.components/render-control-block-result`!
  ;; Here's how to do it:

  ;; define the render method.
  ;; give it a distinct key to dispatch on.
  (defmethod components/render-control-block-result :my-table
    [{:keys [state] :as control-block} oob?]
    (let [;; deref the state to get the value of the `letcontrols` body
          result @state
          ;; pull any metadata you've added
          {:keys [title]} (meta result)]
      ;; you don't need to use this, but it's helpful to
      ;; guarantee the correct shape required for htmx to work.
      (components/render-control-block-result*
        ;; pass the control block in as-is.
        control-block
        ;; wrap the result in your rendering fn
        (make-table result title)
        oob?)))

  (c/letcontrols
    [my-table-data [{:A 1 :B 2 :C 3}
                    {:A 1 :B 2 :C 3}
                    {:A 1 :B 2 :C 3}
                    {:A 1 :B 2 :C 3}]]
    (with-meta
     my-table-data
     {:result-type :my-table
      ;; you can put whatever data you want into the metadata map
      :title "My Table 3"}))


  )
