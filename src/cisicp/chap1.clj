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

;; Ex 1.11
; f(n) = n | n < 3
; f(n) = f(n - 1) + 2 * f(n - 2) + 3 * f(n - 3) | n >= 3
(defn f-rec [n]
  (if (< n 3)
    n
    (+ (f-rec (- n 1))
       (* 2 (f-rec (- n 2)))
       (* 3 (f-rec (- n 3))))))

; ignoring negative values
(defn f-iter [n]
  (defn go [a b c count]
    (if (= count 0)
      c
      (recur (+ a (* 2 b) (* 3 c))
             a
             b
             (dec count))))
  (go 2 1 0 n))
