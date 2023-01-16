(ns solenoid.controls-test
  (:require [solenoid.controls :as c]
            [clojure.test :as t :refer [deftest testing is run-tests]])
  (:import [solenoid.controls Num Slider Text]))

(deftest infer-controls-test
  (testing "controls can be inferred."
    (is (instance? Num    (c/infer-control 2)))
    (is (instance? Text   (c/infer-control "some text")))
    (is (instance? Slider (c/infer-control {:type :slider :value 2})))))
