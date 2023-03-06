#?@(:bb
  [#!/usr/bin/env bb
   (require '[babashka.deps :as deps])
   (deps/add-deps
     '{:deps {adam-james-v/solenoid {:git/url "https://github.com/adam-james-v/solenoid/"
                                     :sha "85ce76eadefdc3793d59a40e031b1eb5cc99942c"}}})

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

(ns solenoid.example-script
  (:require [clojure.string :as str]
            [solenoid.server :as ss]
            [solenoid.controls :as c]))

(ss/serve!)        ;; start the server at http://localhost:9876
#_(ss/serve! 9877) ;; or some other port if you'd like

;; ------ some geometry functions --------

(defn round
  "Rounds a non-integer number `num` to `places` decimal places."
  ([num]
   (round num 5))
  ([num places]
   (if places
     (let [d (bigdec (Math/pow 10 places))]
       (double (/ (Math/round (* (double num) d)) d)))
     num)))

(defn line
  "Create a parametric function representing a straight line, with no checks."
  [[ax ay :as a] b]
  (let [[vx vy] (mapv - b a)]
    (fn [t]
      [(+ ax (* vx t))
       (+ ay (* vy t))])))

(defn quadratic-bezier
  [a b c]
  (fn [t]
    (let [l1 (line a b)
          l2 (line b c)
          l3 (line (l1 t) (l2 t))]
      (l3 t))))

(defn bezier
  [pts]
  (if (= 3 (count pts))
    (apply quadratic-bezier pts)
    (let [lines (map #(apply line %) (partition 2 1 pts))]
      (fn
        [t]
        (let [npts (map #(% t) lines)]
          ((bezier npts) t))))))

(def curvy
  (c/letcontrols
    [start-x   {:type :slider :value 125 :min 0 :max 250 :display-name "START-X"}
     end-x     {:type :slider :value 125 :min 0 :max 250 :display-name "END-X"}
     height    {:type :slider :value 250 :min 0 :max 500 :display-name "HEIGHT"}
     cptsa     {:type :edn :value [[20 130] [200 0]] :display-name "A Control Pts"}
     cptsb     {:type :edn :value [[20 240] [300 0]] :display-name "B Control Pts"}
     show-cpts {:type :edn :value [true] :display-name "SHOW CONTROL POINTS"}
     n         {:type :slider :value 16 :min 5 :max 175 :display-name "Segments"}
     stroke    {:type :text :value "white" :display-name "STROKE COL"}
     fill      {:type :text :value "none" :display-name "FILL COL"}]
    (let [step    (/ 1.0 n)
          curve-a (bezier (concat [[start-x 0]] cptsa [[end-x height]]))
          curve-b (bezier (concat [[start-x 0]] cptsb [[end-x height]]))]
      [:svg {:width 250 :height 500}
       (when (first show-cpts)
           (into [:g ]
                 (mapv (fn [[x y]]
                         [:circle {:cx x :cy y :r 3 :fill "red"}]) (concat cptsa cptsb))))
       (let [a   (map curve-a (range 0 1 step))
             b   (reverse (map curve-b (range 0 1 step)))
             pts (concat a b)]
         [:polygon
          {:points       (str/join " " (map #(str/join "," %) pts))
           :stroke       stroke
           :fill         fill
           :stroke-width 1}])
       (into [:g]
             (mapv (fn [t]
                     (let [[x1 y1] (curve-a t)
                           [x2 y2] (curve-b t)]
                       [:line {:x1           x1 :y1 y1
                               :x2           x2 :y2 y2
                               :stroke       stroke
                               :stroke-width 0.5}])) (range 0 1 step)))])))

;; remember you can always @curvy to get at the value at any point in time.
;; You could do that to save the results of configurations you like with (def save-1 @curvy)
;; and then run the hiccup compiler and save that to an svg file.

(comment

  (def save-1 @curvy)
  (spit "nice.svg" (hiccup.core/html save-1))

)


;; when running outside of the repl, (eg. `bb example-script.clj` in terminal)
;; this is necessary to prevent bb from closing right away
(when (= *file* (System/getProperty "babashka.file"))
  @(promise))
