(ns cisicp.chap1)

;; Ex 1.8
(defn cube-root [x]
  (defn square [y]
    (* y y))
  (defn cube [y]
    (* y y y))
  (defn good-enough? [guess]
    (< (Math/abs (- (cube guess) x)) 0.001))
  (defn improve [guess]
    (/ (+ (/ x (square guess))
          (* 2 guess))
       3))
  (defn cube-root-iter [guess]
    (if (good-enough? guess)
      guess
      (recur (improve guess))))
  (cube-root-iter 1.0))
