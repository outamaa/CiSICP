(ns cisicp.chap1-test
  (:require [clojure.test :refer :all]
            [cisicp.chap1 :refer :all]))

(defn within-eps? [x y eps]
  (< (Math/abs (- x y)) eps))

(deftest cube-root-test
  (testing "cube root of 27 = 3"
    (is (within-eps? (cube-root 27)
                     3
                     0.001))))

(deftest ex-1.11-test
  (testing "f-iter and f-rec should give same answers"
    (doseq [n (range 15)]
      (is (= (f-iter n) (f-rec n))))))
