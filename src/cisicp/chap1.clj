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

;; Ex 1.12
; A recursive solution
(defn pascal [n]
  (if (<= n 0)
    [[1]]
    (let [pascal-dec (pascal (dec n))
          pascal-last (last pascal-dec)
          pascal-n (vec (map +
                             (concat [0] pascal-last)
                             (concat pascal-last [0])))]
      (conj pascal-dec pascal-n))))

; As a bonus, here's a lazy seq of the 'rows' of Pascal triangle
(defn pascal-seq
  ([] (pascal-seq [1]))
  ([pascal-n]
   (let [pascal-n-inc (vec (map +
                                (concat [0] pascal-n)
                                (concat pascal-n [0])))]
     (cons pascal-n (lazy-seq (pascal-seq pascal-n-inc))))))

;; Ex 1.16
(defn fast-expt-iter [b n]
  (defn iter-loop [b n a]
    (cond
      (zero? n) a
      (odd? n) (recur b (dec n) (* b a))
      :else (recur (* b b) (/ n 2) a)))
  (iter-loop b n 1))
