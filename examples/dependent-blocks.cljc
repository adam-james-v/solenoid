#?@(:bb
  [#!/usr/bin/env bb
   (require '[babashka.deps :as deps])
   (deps/add-deps
     '{:deps {adam-james-v/solenoid {:git/url "https://github.com/adam-james-v/solenoid/"
                                     :sha "c0ab90c73fbc61fa25f24b83b13e3be4d81dab1a"}}})

   (let [[major minor patch] (->> (clojure.string/split (System/getProperty "babashka.version") #"\.")
                                  (map parse-long))]
     (when-not (and (>= major 1)
                    (>= minor 2)
                    (>= patch 174))
       (do (println
             (format "Your Babashka Version is : %s, but version 1.2.174 Or Newer is required."
                     (System/getProperty "babashka.version"))
             "This is the case because Solenoid relies on atom's .getWatch, which is provided in Babashka 1.2.174.")
           (System/exit 1))))])

(ns dependent-blocks
  (:require [solenoid.server :as ss]
            [solenoid.controls :as c]
            [solenoid.components :as components]))

;; Let's start the server, preferring port 9876.
;; If the port is already in use, the server will pick an available port for you, starting at 8000 and looking up from there.

;; You may also notice that there's some custom Hiccup in the `:head-entries` key.
;; This is how you can customize what scripts and css are loaded.

;; This script is a cool one (in my opinion) partly due to the cool 3D features the X_ITE project
;; provides. Check out https://create3000.github.io/x_ite/ if you want to learn more.
;; Since Solenoid is built with HTMX, any library that lets you write hypertext markup inside your html pages (think embedded SVG)
;; becomes available for us to play with! Construct the appropriate hiccup on the server side and send it over
;; and HTMX should handle the rest nicely :).

(ss/serve! {:port 9876
            :head-entries [[:script {:src "https://create3000.github.io/code/x_ite/latest/x_ite.min.js"}]]})

;; In the custom-control-block-render-example.clj file, I show you how to create a custom
;; table rendering method. In that file, I only showed a `components/render-control-block-result` method
;; because it was all that was needed. The table render was already normal html (as hiccup).

;; For the x3D rendering, we need to do a bit more work to have things load properly. We'll also
;; need to create a custom `components/render-control-block` function that will be responsible for
;; properly setting up the x3D container that the result will be rendered into. Properly placing the
;; necessary containing element (in this case an `:x3d-canvas` with a nested `:Scene`) lets us load
;; changed results without reloading the container, which means we don't have to reload/restart the scene
;; on ever change.

;; If you instead create this canvas element inside just the result block, things will work, but you'll
;; notice that every change causes the whole scene to reload. This isn't a very enjoyable experience.
;; The added step of creating one more method is well worth the bit of effort to get it right!

(defmethod components/render-control-block-result :x3d
  [{:keys [id state]}]
  (let [result @state]
    [:Group.text-center
     (merge
       {:id          (str (name id) "-result")
        :class       ["control-block-result"]
        :hx-swap-oob "morphdom"})
     (or result "no result")]))

(defn x3d-result-wrap-fn
  [result]
  [:x3d-canvas
   {:style {:margin "0 auto"
            :width "100%" #_"350px"
            :height "400px"}}
   [:X3D {:profile "Full"
          :version "8.5.2"}
    [:Scene
     [:Background {:skyColor    "0.2 0.2 0.2"
                   #_#_:transparency "1"}]
     [:ViewpointGroup
      [:Viewpoint {:description "Perspective"
                   :position    "500 500 500"
                   :viewAll     "true"}]]
     result]]])

(defmethod components/render-control-block :x3d
  [control-block]
  ;; so that you don't have to know the required structure, a base impl function is used
  (components/render-control-block*
    control-block
    (x3d-result-wrap-fn
      ;; here you'll just call the result render multi method so that dispatch will work.
      ;; You still need to define the result rendering separately because `render-control-block-result`
      ;; runs outside of this context as well. In particular, any time a controller changes, the result render
      ;; runs but the `render-control-block` doesn't.
      (components/render-control-block-result control-block))))

;; OK! That's all of the custom work we need. Now we can build out some cool examples!

;; I've got a few utility functions for 2D/3D stuff:

(defn round
  "Rounds a non-integer number `num` to `places` decimal places."
  ([num] (round num 5))
  ([num places]
   (if places
     (let [d (bigdec (Math/pow 10 places))]
       (double (/ (Math/round (* (double num) d)) d)))
     num)))

(defn distance
  "Computes the distance between two points `a` and `b`."
  [a b]
  (let [v (mapv - b a)
        v2 (reduce + (mapv * v v))]
    (round (Math/sqrt ^double v2))))

(defn regular-polygon-pts
  "Return a list of points making up a polygon with distance to the points `r` and `n` edges."
  [r n]
  (let [angle (* 2 Math/PI (/ 1 n))]
    (map #(vector (round (* r (Math/cos (* % angle))) 5)
                  (round (* r (Math/sin (* % angle))) 5))
         (range n))))

(defn average
  "Compute the average of `numbers`."
  [& numbers]
  (let [n (count numbers)]
    (round (/ (apply + numbers) n))))

(defn centroid-of-pts
  "Calculates the arithmetic mean position of the given `pts`."
  [pts]
  (let [ndim (count (first (sort-by count pts)))
        splits (for [axis (range 0 ndim)]
                 (map #(nth % axis) pts))]
    (mapv #(apply average %) splits)))

(defn- remap-within
  "Shift the parameter range of `f` from 0 to 1 to `start` to `end`."
  [f [start end] x]
  (when (and (>= x start) (< x end))
    (let [step (- end start)
          t (/ (- x start) step)]
      (f t))))

(defn line
  "Create a parametric function representing a straight line.
  The returned function takes a parameter `t` between 0 and 1, where t 0 = `a` and t 1 = `b`."
  [a b]
  (fn
    ([] {:input [a b]
         :origin (centroid-of-pts [a b])
         :vertex-params [0 1]
         :length (distance a b)})
    ([t]
     (cond
       (= (float t) 0.0) a
       (= (float t) 1.0) b
       :else
       (mapv + a (mapv * (mapv - b a) (repeat t)))))))

(defn circle
  "Create a parametric function representing a circle with radius `r` centered at the origin, or circumscribing points `a`, `b`, and `c`, as long as the three points are not colinear.
  The returned function takes a parameter `t` between 0 and 1, where t 0 and 1 = [r 0] or centroid + calcuated radius."
  [r]
  (fn
    ([] {:input [r]
         :origin [0 0]
         :vertex-params [0]
         :length (* Math/PI 2 r)})
    ([t]
     (let [t (* 2 Math/PI t)
           x (* r (Math/cos t))
           y (* r (Math/sin t))]
       [x y]))))

(defn polygon
  "Create a parametric function representing a polygon with straight segments defined by `pts`.
  The returned function takes a parameter `t` between 0 and 1, where t 0 and 1 = (first `pts`)."
  [pts]
  (let [pts (concat (vec pts) [(first pts)])
        lines (map (partial apply line) (partition 2 1 pts))
        length (reduce + (map #(:length (%)) lines))
        intervals (->> lines
                       (map #(:length (%)))
                       (reductions +)
                       (concat [0])
                       (map #(/ % length))
                       (partition 2 1))]
    (fn
      ([] {:input [pts]
           :origin (centroid-of-pts pts)
           :vertex-params (concat [0] (mapv second intervals))
           :length (reduce + (map #(:length (%)) lines))})
      ([t]
       (cond
         (= (float t) 0.0) (first pts)
         (= (float t) 1.0) (last pts)
         :else
         (first
          (filter some?
                  (map #(remap-within %1 %2 t) lines intervals))))))))

(defn line3d
  [pts]
  (let [pts       (mapv (fn [[x y z]]
                          (if (not z)
                            [x y 0]
                            [x y z])) pts)
        stringify (fn [pts]
                    (->> pts
                         (interpose [","])
                         (apply concat)
                         (map str)
                         (interpose " ")
                         (apply str)))
        profile   (->> (regular-polygon-pts 0.75 16) (mapv (fn [[x y]] [x 0 y])))
        profile   (concat profile [(first profile)])]
    [:Shape
     [:Appearance
      [:Material {:diffuseColor  "2,2,2"
                  :emissiveColor "2,2,2"
                  :creaseAngle   "2"}]]
     [:Extrusion {:ccw          "true"
                  :solid        "true"
                  :convex       "true"
                  :beginCap     "true"
                  :endCap       "true"
                  :crossSection (stringify profile)
                  :spine        (stringify pts)}]]))


(defn fastline
  "Create a parametric function representing a straight line, with no checks and slightly faster implementation meant primarily for use in the bezier implementation.
  The returned function takes a parameter `t` between 0 and 1, where t 0 = `a` and t 1 = `b`."
  [a b]
  (fn [t]
    (mapv (fn [da db] (+ da (* (- db da) t))) a b)))

(defn arc-length
  "Calculate the arc length of `curve`, being exact where possible and estimating otherwise.
  For example, bezier curves are estimated, but circles and arcs have exact results (barring rounding)."
  ([curve] (arc-length curve 0 1))
  ([curve t] (arc-length curve 0 t))
  ([curve ta tb]
   (let [seg   200 ;; keep this number kinda low to keep speeds up. It's not a great solution...
         start (/ (* ta seg) seg)
         end   (/ (inc (* tb seg)) seg)
         ts    (range start end (/ 1 seg))]
     ;; try a transducer approach. Probably room for improvements in this whole thing, honestly :)
     (transduce (comp
                  (map curve)
                  (partition-all 2)
                  (remove #(= 1 (count %)))
                  (map #(apply distance %)))
                +
                (interleave ts (rest ts)))
     #_(->> (range start end (/ 1 seg))
          (map curve)
          (partition 2 1)
          (map #(apply distance %))
          (reduce +)
          (#(round % 5))))))

(defn- quadratic-bezier
  [a b c]
  (fn [t]
    (let [l1 (fastline a b)
          l2 (fastline b c)
          l3 (fastline (l1 t) (l2 t))]
      (l3 t))))

(defn- bezier*
  [pts]
  (if (= 3 (count pts))
    (apply quadratic-bezier pts)
    (let [lines (map #(apply fastline %) (partition 2 1 pts))]
      (fn
        [t]
        (let [npts (map #(% t) lines)]
          ((bezier* npts) t))))))

(defn bezier
  "Create a parametric function representing a bezier curve with control points `pts`, as long as there are at least 3 points.
  The returned function takes a parameter `t` between 0 and 1, where t 0 = (first `pts`) and t 1 = (last `pts`)."
  [pts]
  (when (> (count pts) 2)
    (let [curve (bezier* pts)
          length (arc-length curve)]
      (fn
        ([] {:fn `bezier
             :input [pts]
             :origin (centroid-of-pts pts)
             :vertex-params [0 1]
             :length length})
        ([t] (curve t))))))

;; Here, we'll define 2 sets of points. We can set up some basic controls to change the output.
;; Remember that at this time, to pass just values back through derefs, you'll have to put stuff in the
;; meta.

;; So, wrap the hiccup in `with-meta` and add meaningful keys/vals to be able to reach them later.

(def pts-a
  (c/letcontrols
    [r {:display-name "R" :type :slider :value 10 :min 1 :max 200 :step 0.1}
     n {:display-name "N" :type :num :value 8 :min 3 :max 200 :step 1}]
    (let [pts (regular-polygon-pts r n)
          [w h] [150 150]
          pts-str (->> pts
                       (map (fn [[x y]] (str x "," y)))
                       (interpose [" "])
                       (apply concat)
                       (apply str))]
      (with-meta
       [:svg {:viewBox (format "%s %s %s %s" (int (* w -0.5)) (int (* h -0.5)) w h)
              :width w
              :height h}
        [:polygon {:points pts-str
                   :fill "none"
                   :stroke "cyan"
                   :stroke-width 2}]]
       {:pts pts}))))

(def pts-b
  (c/letcontrols
    [r {:display-name "R" :type :slider :value 40 :min 1 :max 200 :step 0.1}
     n {:display-name "N" :type :num :value 6 :min 3 :max 200 :step 1}]
    (let [pts (regular-polygon-pts r n)
          [w h] [150 150]
          pts-str (->> pts
                       (map (fn [[x y]] (str x "," y)))
                       (interpose [" "])
                       (apply concat)
                       (apply str))]
      (with-meta
       [:svg {:viewBox (format "%s %s %s %s" (int (* w -0.5)) (int (* h -0.5)) w h)
              :width w
              :height h}
        [:polygon {:points pts-str
                   :fill "none"
                   :stroke "cyan"
                   :stroke-width 2}]]
       {:pts pts}))))

(defn stringify
  [pts]
  (->> pts
       (interpose [","])
       (apply concat)
       (map str)
       (interpose " ")
       (apply str)))

;; This is the first dependent block!
;; Inside the body, we grab pts-a with `(-> @pts-a meta :pts reverse)`, and pts-b similarly.
;; The macro finds those derefs and connects our block's result rendering to changes in
;; those values too. This means that adjusting the controls for pts-a and pts-b will result in
;; changes to this block too! I think that's super cool.

;; In this block, I've just set up two sliders to control the start/end positions of the curve,
;; as well as a value N which is the number of samples to take along the parametric curve
;; that I define as `poly` in the let binding.
;; Parametric curves are just functions that take a value `t` from 0 to 1 and returns a point (2D or 3D depending on the curve).
;; It's a useful technique for 3D design, and it's also a lot of fun to play around with.

(def donut
  (c/letcontrols
    [t1 {:display-name "R" :type :slider :value 0 :min 0 :max 1 :step 0.0001}
     t2 {:display-name "R" :type :slider :value 1 :min 0 :max 1 :step 0.0001}
     n 100]
    (let [pts-a             (-> @pts-a meta :pts reverse)
          pts-a             (concat pts-a [(first pts-a)])
          poly              (->> @pts-b meta :pts reverse polygon) ;; this is a parametric curve.
          pts-b             (->> (map poly (range t1 t2 (/ 1.0 n))) (mapv (fn [[x y]] [x 0 y])))
          pts-b             (if (and (<= t1 0.0001) (>= t2 0.9999))
                              (concat pts-b [(first pts-b)])
                              pts-b)
          cross-section-str (stringify pts-a)
          spine-str         (stringify pts-b)]
      (with-meta
       [:Shape
        [:Appearance
         [:Material {:diffuseColor  "green"
                     :emissiveColor "green"}]]
        [:Extrusion {:ccw          "true"
                     :solid        "true"
                     :convex       "true"
                     :beginCap     "true"
                     :endCap       "true"
                     :crossSection cross-section-str
                     :spine        spine-str}]]
       {:control-block-type :x3d
        :result-type        :x3d}))))

;; OK, now let's play around with a different idea... using Bezier Curves.
;; I won't go into the math, I'll just set up several control blocks to showcase how Solenoid can be used.

;; Here is a potentially useful idea: Control blocks with NO rendered output (display 'none'),
;; but pass the control values along in the meta as a map.

(def control-circle-params
  (c/letcontrols
    [r1 {:display-name "R1" :type :slider :value 30 :min 0 :max 500 :step 0.01}
     r2 {:display-name "R2" :type :slider :value 30 :min 0 :max 500 :step 0.01}
     r3 {:display-name "R3" :type :slider :value 30 :min 0 :max 500 :step 0.01}
     r4 {:display-name "R4" :type :slider :value 39 :min 0 :max 500 :step 0.01}
     h1 {:display-name "H1" :type :slider :value 0 :min 0 :max 500 :step 0.01}
     h2 {:display-name "H2" :type :slider :value 50 :min 0 :max 500 :step 0.01}
     h3 {:display-name "H3" :type :slider :value 100 :min 0 :max 500 :step 0.01}
     h4 {:display-name "H4" :type :slider :value 200 :min 0 :max 500 :step 0.01}]
    (with-meta
     [:span {:display "none"} ""]
     {:data {:r1 r1
             :r2 r2
             :r3 r3
             :r4 r4
             :h1 h1
             :h2 h2
             :h3 h3
             :h4 h4}})))

;; Then, you can use those values in other blocks:

(def control-point-params
  (c/letcontrols
    [t1 {:display-name "T1" :type :slider :value 0.115 :min 0 :max 1 :step 0.001}
     t2 {:display-name "T2" :type :slider :value 0.252 :min 0 :max 1 :step 0.001}
     t3 {:display-name "T3" :type :slider :value 0.735 :min 0 :max 1 :step 0.001}
     t4 {:display-name "T4" :type :slider :value 0.130 :min 0 :max 1 :step 0.001}]
    (let [{:keys [r1 r2 r3 r4 h1 h2 h3 h4]} (-> @control-circle-params meta :data)
          add-h (fn [c h]
                  (fn [t]
                    (let [[x y] (c t)]
                      [x h y])))
          [c1 c2 c3 c4 :as cs] (map add-h (map circle [r1 r2 r3 r4]) [h1 h2 h3 h4])
          f (fn [c]
              (->> (map c (range 0 1.05 0.05))
                   line3d))
          ptfn (fn [[x y z]]
                 [:Transform {:translation (format "%s, %s, %s" x y z)}
                  [:Shape
                   [:Appearance
                    [:Material {:diffuseColor "red"
                                :emissiveColor "red"}]]
                   [:Sphere {:radius 4}]]])
          control-pts (map (fn [c t] (c t)) [c1 c2 c3 c4] [t1 t2 t3 t4])]
      (with-meta
       (into [:Group]
             (concat
               (map f cs)
               (map ptfn control-pts)
               [(line3d (map (bezier control-pts) (range 0 1.05 0.05)))]))
       {:control-block-type :x3d
        :result-type        :x3d
        :control-pts control-pts }))))

;; Now, let's make something cool!
;; Extrude the shape from pts-a along the bezier from the previous 2 control blocks.

;; And, play with t values too to see what happens :)

(defn distribution-fn
  [st spread]
  (fn [t]
    (Math/pow 2
              (* (/ -105 spread)
                 (Math/pow (- t 0.0 st) 2)))))

(defn spread-scale
  [t t2 scale spread ts]
  (let [spread-scales (map (distribution-fn t spread) ts)]
    (->> (map #(+ t2 (* scale %)) spread-scales)
         (mapv (fn [v] (mapv double [v v]))))))

(def curve
  (c/letcontrols
    [steps {:display-name "STEPS" :type :num :value 30 :min 10 :max 200 :step 1}
     t1 {:display-name "T1" :type :slider :value 0 :min 0 :max 1 :step 0.0001}
     sc {:display-name "SCALE" :type :slider :value 0.001 :min 1 :max 10 :step 0.0001}
     sp {:display-name "SPREAD" :type :slider :value 1 :min 0.001 :max 10 :step 0.0001}
     t4 {:display-name "T4" :type :slider :value 1 :min 0 :max 1 :step 0.001}]
    (let [c       (bezier (-> @control-point-params meta :control-pts)) ;; parametric curve from the bezier points
          ts      (range 0 1 (double (/ 1 (or steps 10))))
          ts      (if (< (apply max ts) 1)
                    (conj (vec ts) 1)
                    ts)
          pts     (mapv (fn [[x y z]]
                          (if (not z)
                            [x y 0]
                            [x y z])) (map c ts))
          profile-pts (->> @pts-a meta :pts reverse)
          profile (concat profile-pts [(first profile-pts)])
          scales  (spread-scale t1 t4 sc sp ts)]
      (with-meta
       [:Shape
        [:Appearance
         [:Material {:diffuseColor      "salmon"
                     :creaseAngle       "0"}]]
        [:Extrusion {:ccw          "true"
                     :solid        "true"
                     :convex       "true"
                     :beginCap     "true"
                     :endCap       "true"
                     :scale        (stringify scales)
                     :crossSection (stringify profile)
                     :spine        (stringify pts)}]]
        {:control-block-type :x3d
         :result-type        :x3d}))))

;; when running outside of the repl, (eg. `bb example-script.clj` in terminal)
;; this is necessary to prevent bb from closing right away
(when (= *file* (System/getProperty "babashka.file"))
  @(promise))
