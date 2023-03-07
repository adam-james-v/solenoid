# solenoid

Solenoid implements a `letcontrols` let-style macro that allows you to wrap any code body in simple, live controls. It is a lightweight tool you can reach for while playing with code at the repl.

I think the utility of this library is best shown, rather than explained.

Here's a cool showcase:

https://user-images.githubusercontent.com/21064735/223320630-cd15ad31-65bb-4995-8b79-6383db36f202.mp4

Here's a simple copy-pastable example:

```clojure
(ns pencil.scratch
  (:require [solenoid.controls :as c]
            [solenoid.server :as ss]))

(ss/serve!) ;; webpage served on port 9876, or lowest available port starting at 8000.

(def example-1
  (c/letcontrols
    [a 1
     b 2]
    (+ a b)))

;; If you have your browser open to http://localhost:9876, you should
;; see a 'control block' show up with number inputs inferred for each letcontrol binding

@example-1 ;; => 3 before any changes in the UI

;; Make some changes in the UI and de-ref example-1 again.
;; You should see the changes reflected right away, without
;; needing to refresh your browser or anything. Pretty sweet, if you ask me 😊
```

Here's the UI you should see:

![Screenshot of the UI for example-1, with two number inputs and the result displayed below the controls](https://github.com/adam-james-v/solenoid/blob/main/images/example-1.png "Example-1 UI Screenshot")

This is a 'control block'. All control blocks have the same structure:
 - An ID is generated and displayed in the top bar. In this screenshot, the control block has an id of `:controlblock-11949` (displayed as ` controlblock-11949` in the UI).
 - A remove button: 'X' in the top right corner. This won't un-def anything in your repl, so be a bit careful with this, although `@example-1` should still return the last valid value before you removed the control block
 - The set of controls. Each control has a generated ID on the left, the input in the middle, and the value of that input on the right, if relevant. The number inputs do not render the value as you can see it directly inside the input itself.
 - the rendered results. There is nothing smart about the rendering that occurs here other than the value of the body inside the `letcontrols` being passed into the hiccup renderer. So, just be careful, especially if the body resolves to a vector value. Wrap it in a `str` to allow it to render! This is an area I'll be working to improve, but for now, it is the user's responsibility.
 - a `def` button in the bottom left. This will def the current result value to `user/controlblockID`, where you can do whatever you want with it.

But what if you want to have some more meaningful labels to work with? You can pass in a map to set up the controls more specifically. The following example shows all of the currently available control types: :num, :slider, :text, and :edn. That's a short list, I know, but at least :edn gives you the ability to put basically any structure in there, so it's decently powerful already.

```clojure
(def example-2
  (c/letcontrols
    [;; If you are setting up a control with a map, :type and :value are required, :display-name is optional
     a {:type :num :value 140 :display-name "Num A"}
     b {:type :num :value 140 :display-name "Num B"}
     ;; sliders require :min and :max, but :step is optional
     c {:type :slider :value 140 :min 0 :max 1 :step 0.01 :display-name "Factor"}
     d {:type :edn :value [1 2] :display-name "More Numbers"}
     e [2 3]
     f "My Result: "]
    ;; make sure your result can successfully pass through the hiccup compiler!
    [:p (str f (apply * (concat [a b c] d e)))]))

@example-2 ;; change in the UI and deref again!
```

Here is the control block's UI. Notice that I changed some values!

![Screenshot of the UI for example-2, with number inputs, a slider, and some text inputs, with the result displayed beneath them.](https://github.com/adam-james-v/solenoid/blob/main/images/example-2.png "Example-2 UI Screenshot")

As you play around with this tool, it might happen that you create some invalid results. As much as possible, I want this to be ok and not break the UI. If you 'break' things by typing out new edn values, things _should_ recover once you re-establish valid values, but just be aware that things may not be bulletproof (yet).

If things are really weird, you can always clear the controls registry as follows:

```clojure
(reset! c/registry {})
```

Which eliminates all control blocks and controls. If you know exactly which keys and values you want to change you can of course try to perform some value surgery on the registry, it's just a normal atom, and you can change things if you'd like!

I find that if I'm playing around in a repl anyway, it's not usually too devastating to wipe the registry to make sure everything's clean, but do what you wish!

## Fun Examples
You can check out some examples in the [examples directory](https://github.com/adam-james-v/solenoid/tree/main/examples), but I've pasted one here too:

Here's a fun example that works as a Babashka script. (Requires version `1.2.174` or newer).

A screenshot of the UI first:

![Screenshot of the UI for example-3, with number inputs, a slider, and some text inputs, with the result displayed beneath them.](https://github.com/adam-james-v/solenoid/blob/main/images/example-3.png "Example-3 UI Screenshot")

Below is the script I used to create this. It can also be found in the examples folder of this project.

To run this on its own:

```bash
cd examples
bb example-script.clj
```

And navigate to `http://localhost:9876`.

To run things in a repl, you can:

```bash
bb -nrepl-server # or your preferred repl server
```

Connect to the repl server however your editor allows. I use emacs and so can do:

`M-x cider-connect RET localhost RET 9876`

Then, open the `examples/examples-script.clj` file and evaluate. Have fun!

```clojure
#!/usr/bin/env bb
(require '[babashka.deps :as deps])
(deps/add-deps
  '{:deps {adam-james-v/solenoid {:git/url "https://github.com/adam-james-v/solenoid/"
                                  ;; MAYBE USE LATEST SHA HERE
                                  :sha "029d8a66c11141b94fe9be3487236beb9c325197"}}})

(ns solenoid.example
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

```
