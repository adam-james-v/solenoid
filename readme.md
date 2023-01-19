# solenoid

Solenoid implements a `letcontrols` let-style macro that allows you to wrap any code body in simple, live controls.

I'll update this readme with a real example shortly. Here's a trivial one to get things started:

```clojure
(ns pencil.scratch
  (:require [solenoid.controls :as c]
            [solenoid.server :as ss]))

(ss/serve!) ;; webpage served on port 9876


(def example
  (c/letcontrols [a {:type :num :value 140 :display-name "Base R"}
                  b {:type :num :value 140 :display-name "Top R"}
                  c [1 2]]
                (apply * (concat [a b] c))))

@example ;; change in the UI and deref again!

```
