(ns dependent-blocks
  (:require [solenoid.server :as ss]
            [solenoid.controls :as c]
            [solenoid.components :as components]))

(ss/serve! {:port 9876
            :head-entries [[:script {:src "https://create3000.github.io/code/x_ite/latest/x_ite.min.js"}]]})

;; try X_ITE X3D in html, using custom render block/result methods
;; https://create3000.github.io/x_ite/playground/
;; X3D nodes: https://graphics.stanford.edu/projects/iwork/old/x2d/nodes.html

(defmethod solenoid.components/render-control-block-result :x3d
  [{:keys [id state]} oob?]
  (let [result @state]
    [:Group.text-center
     (merge
       {:id          (str (name id) "-result")
        :class       "control-block-result"}
       (when oob? {:hx-swap-oob "innerHTML"}))
     (or result "no result")]))

(defn x3d-result-wrap-fn
  [result]
  [:x3d-canvas
   {:style {:width "350px"
            :height "250px"}}
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

(defmethod solenoid.components/render-control-block :x3d
  [control-block]
  (solenoid.components/render-control-block*
    control-block
    (x3d-result-wrap-fn
      (solenoid.components/render-control-block-result control-block false))))

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
    ([] {:fn `line
         :input [a b]
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
    ([] {:fn `circle
         :input [r]
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
      ([] {:fn `polygon
           :input [pts]
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
      [:Material {:diffuseColor "2,2,2"
                  :emissiveColor "2,2,2"
                  :creaseAngle "2"}]]
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
  [[ax ay :as a] b]
  (let [[vx vy vz] (mapv - b a)]
    (fn [t]
      (mapv (fn [da db]
              (+ da (* (- db da) t))) a b)
      #_[(+ ax (* vx t))
       (+ ay (* vy t))])))

(defn arc-length
  "Calculate the arc length of `curve`, being exact where possible and estimating otherwise.
  For example, bezier curves are estimated, but circles and arcs have exact results (barring rounding)."
  ([curve] (arc-length curve 0 1))
  ([curve t] (arc-length curve 0 t))
  ([curve ta tb]
   (let [seg   200
         start (/ (* ta seg) seg)
         end   (/ (inc (* tb seg)) seg)
         ts    (range start end (/ 1 seg))]
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

(defn circle3d
  [])

(comment
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

  (c/letcontrols
    [h 4]
    (let [pts               (-> @pts meta :pts)
          ctr               (centroid-of-pts pts)
          pts               (mapv (fn [pt] (mapv - pt ctr)) pts)
          bottom            (mapv #(conj % 0) pts)
          top               (mapv #(conj % h) pts)
          bottom-indices    (vec (range (count bottom)))
          top-indices       (vec (range (count bottom) (+ (count bottom) (count top))))
          mid-faces-indices (mapv (fn [a b]
                                    (vec (concat a (reverse b) [-1])))
                                  (partition 2 1 (concat bottom-indices [(first bottom-indices)]))
                                  (partition 2 1 (concat top-indices [(first top-indices)])))
          faces-string      (->> (concat [(concat (reverse bottom-indices) [-1])] mid-faces-indices [(concat top-indices [-1])])
                                 (apply concat)
                                 (interpose ",")
                                 (apply str))
          verts-string      (->> (concat bottom top)
                                 (interpose [","])
                                 (apply concat)
                                 (map str)
                                 (interpose " ")
                                 (apply str))]
      (with-meta
       [:Shape
        [:Appearance
         [:Material #_{:diffuseColor "0 0.5 1"}]]
        [:IndexedFaceSet {:ccw        "true"
                          :solid      "true"
                          :convex     "true"
                          :coordIndex faces-string}
         [:Coordinate {:point verts-string}]]]
       {:control-block-type :x3d
        :result-type        :x3d})))


  ;; <Extrusion
  ;; beginCap='true'
  ;; ccw='true'
  ;; convex='true'
  ;; creaseAngle='0'
  ;; crossSection='[(1,1), (1, -1), (-1, -1), (-1, 1), (1, 1)]'
  ;; endCap='true'
  ;; height='0'
  ;; lit='true'
  ;; metadata='X3DMetadataObject'
  ;; orientation='[(0,0,0,1)]'
  ;; scale='[(1,1)]'
  ;; solid='true'
  ;; spine='[(0,0,0)]'
  ;; useGeoCache='true' ></Extrusion>

  (c/letcontrols
    [t1 {:display-name "R" :type :slider :value 0 :min 0 :max 1 :step 0.0001}
     t2 {:display-name "R" :type :slider :value 1 :min 0 :max 1 :step 0.0001}
     n 8]
    (let [stringify         (fn [pts]
                              (->> pts
                                   (interpose [","])
                                   (apply concat)
                                   (map str)
                                   (interpose " ")
                                   (apply str)))
          pts-a             (-> @pts-a meta :pts reverse)
          pts-a             (concat pts-a [(first pts-a)])
          #_#_pts-b         (->> @pts-b meta :pts reverse (mapv (fn [[x y]] [x 0 y])))
          #_#_pts-b         (concat pts-b [(first pts-b)])
          poly              (->> @pts-b meta :pts reverse polygon)
          pts-b             (->> (map poly (range t1 t2 (/ 1.0 n))) (mapv (fn [[x y]] [x 0 y])))
          pts-b             (if (and (<= t1 0.0001) (>= t2 0.9999))
                              (concat pts-b [(first pts-b)])
                              pts-b)
          cross-section-str (stringify pts-a)
          spine-str         (stringify pts-b)]
      (with-meta
       [:Shape
        [:Appearance
         [:Material #_{:diffuseColor "0 0.5 1"}]]
        [:Extrusion {:ccw          "true"
                     :solid        "true"
                     :convex       "true"
                     :beginCap     "true"
                     :endCap       "true"
                     :crossSection cross-section-str
                     :spine        spine-str}]]
       {:control-block-type :x3d
        :result-type        :x3d})))

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

  (def control-point-params
    (c/letcontrols
      [t1 {:display-name "T1" :type :slider :value 0 :min 0 :max 1 :step 0.0001}
       t2 {:display-name "T2" :type :slider :value 0 :min 0 :max 1 :step 0.0001}
       t3 {:display-name "T3" :type :slider :value 0 :min 0 :max 1 :step 0.0001}
       t4 {:display-name "T4" :type :slider :value 0 :min 0 :max 1 :step 0.0001}]
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

  (def curve
    (c/letcontrols
      [t1 {:display-name "T1" :type :slider :value 0 :min 0 :max 1 :step 0.0001}
       t2 {:display-name "T2" :type :slider :value 0 :min 0 :max 1 :step 0.0001}
       t3 {:display-name "T3" :type :slider :value 0 :min 0 :max 1 :step 0.0001}
       t4 {:display-name "T4" :type :slider :value 0 :min 0 :max 1 :step 0.0001}]
      (let [c         (bezier (-> @control-point-params meta :control-pts))
            pts       (mapv (fn [[x y z]]
                              (if (not z)
                                [x y 0]
                                [x y z])) (map c (range 0 1.01 0.01)))
            stringify (fn [pts]
                        (->> pts
                             (interpose [","])
                             (apply concat)
                             (map str)
                             (interpose " ")
                             (apply str)))
            profile   (->> @pts-a meta :pts reverse #_(mapv (fn [[x y]] [x y 0])))
            profile   (concat profile [(first profile)])]
        (with-meta
         [:Shape
          [:Appearance
           [:Material {:diffuseColor  "slategray"
                       :emissiveColor "slategray"
                       :creaseAngle   "0"}]]
          [:Extrusion {:ccw          "true"
                       :solid        "true"
                       :convex       "true"
                       :beginCap     "true"
                       :endCap       "true"
                       :crossSection (stringify profile)
                       :spine        (stringify pts)}]]
         {:control-block-type :x3d
          :result-type        :x3d}))))
  )
