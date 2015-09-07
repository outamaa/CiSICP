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

(deftest ex-1.12-test
  (testing "pascal-seq and pascal should produce same(ish) results"
    (doseq [n (range 15)]
      (is (= (into [] (take (inc n) (pascal-seq)))
             (pascal n))))))

(deftest ex-1.16-test
  (testing "fast-expt-iter exponentiates correctly"
    (is (= (fast-expt-iter 2 0) 1))
    (is (= (fast-expt-iter 3 1) 3))
    (is (= (fast-expt-iter 3 3) 27))))

(deftest ex-1.17-test
  (testing "fast-mult multiplies correctly"
    (is (= (fast-mult 2 0) 0))
    (is (= (fast-mult 0 2) 0))
    (is (= (fast-mult 3 1) 3))
    (is (= (fast-mult 3 3) 9))
    (is (= (fast-mult 5 3) 15))))

(deftest ex-1.18-test
  (testing "fast-mult-iter multiplies correctly"
    (is (= (fast-mult 2 0) 0))
    (is (= (fast-mult 0 2) 0))
    (is (= (fast-mult 3 1) 3))
    (is (= (fast-mult 3 3) 9))
    (is (= (fast-mult 5 3) 15))))

(deftest ex-1.19-test
  (testing "fib produces Fibonacci numbers"
    (is (= (fib 1) 1))
    (is (= (fib 2) 1))
    (is (= (fib 3) 2))
    (is (= (fib 4) 3))
    (is (= (fib 5) 5))
    (is (= (fib 23) 28657))))

(deftest ex-1.28-test
  (testing "When using Miller-Rabin test, Carmichael numbers do not pass as prime"
    (is (not (mr-prime? 561 10)))
    (is (not (mr-prime? 1105 10)))
    (is (not (mr-prime? 1729 10))))
  (testing "mr-prime? should return true for primes"
    (is (mr-prime? 2 10))
    (is (mr-prime? 3 10))
    (is (mr-prime? 5 10))
    (is (mr-prime? 3571 10))))

(deftest ex-1.29-test
  (testing "Simpson's Rule integrates correctly"
    (is (= (simpson identity 0 1 100) 1/2))
    (is (= (simpson identity -1 1 100) 0))
    (is (= (simpson #(* % % %) 0 1 100) 1/4))))
