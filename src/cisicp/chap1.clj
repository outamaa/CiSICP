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

;; Ex 1.17
(defn fast-mult [a b]
  (defn double* [x] (* 2 x))
  (defn halve [x] (/ x 2))
  (cond
    (or (zero? a) (zero? b)) 0
    (= a 1) b
    (odd? a) (+ b (fast-mult (dec a) b))
    :else    (fast-mult (halve a) (double* b))))

;; Ex 1.18
(defn fast-mult-iter [a b]
  (defn double* [x] (+ x x))
  (defn halve [x] (/ x 2))
  (defn iter-loop [a b acc]
    (cond
      (zero? a) acc
      (odd? a) (recur (dec a) b (+ b acc))
      :else    (recur (halve a) (double* b) acc)))
  (iter-loop a b 0))

;; Ex 1.19
; (T(p,q))^2 = T(p^2 + q^2, 2pq + q^2)
(defn fib-iter [a b p q count]
  (cond
    (zero? count) b
    (even? count) (recur a
                         b
                         (+ (* p p)
                            (* q q))
                         (+ (* 2 p q)
                            (* q q))
                         (/ count 2))
    :else (recur (+ (* b q) (* a q) (* a p))
                 (+ (* b p) (* a q))
                 p
                 q
                 (dec count))))
(defn fib [n]
  (fib-iter 1 0 0 1 n))

;; Ex 1.28

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (rem a b))))

(defn expmod [base exp m]
  (cond (zero? exp) 1
        (even? exp) (let [exped (expmod base (/ exp 2) m)
                          squared-rem (rem (square exped) m)]
                      (if (and (not= exped 1)
                               (not= exped (dec m))
                               (= squared-rem 1))
                        0
                        squared-rem))
        :else       (rem (* base (expmod base (dec exp) m)) m)))

(defn square [y]
  (* y y))

(defn miller-rabin-test [n]
  (letfn [(try-it [a]
            (= (expmod a (dec n) n) 1))]
    (try-it (inc (rand-int (- n 2))))))

(defn mr-prime? [n times]
  (cond (zero? times) true
        (miller-rabin-test n) (recur n (dec times))
        :else false))

;; Ex 1.29
(defn simpson [f a b n]
  (let [h (/ (- b a) n)]
    (letfn [(multiplier [i]
              (cond (zero? i) 1
                    (= i n)   1
                    (even? i) 2
                    :else     4))
            (y [i]
              (f (+ a (* i h))))]
      (* (/ h 3)
         (apply + (map (comp (partial apply *)
                             (juxt multiplier
                                   y))
                       (range 0 (inc n))))))))

;; Ex 1.30
(defn sum [term a next b]
  (letfn [(iter [a result]
            (if (> a b)
              result
              (recur (next a) (+ result (term a)))))]
    (iter a 0)))

;; Ex 1.31 a
; Let's do product with loop for a more idiomatic Clojure solution.
(defn product [term a next b]
  (loop [curr a
         result 1]
    (if (> curr b)
      result
      (recur (next curr) (* result (term curr))))))

; With 1-based indexing
(defn fac-product [n]
  (product identity 1 inc n))

(defn pi-by-4 [n]
  (letfn [(term [i]
            (/ (if (even? i)
                 (+ i 2)
                 (+ i 3))
               (if (even? i)
                 (+ i 3)
                 (+ i 2))))]
    (float (product term 0 inc n))))

;; Ex 1.31 b
(defn product-rec [term a next b]
  (if (> a b)
    1
    (* (term a)
       (product-rec term (next a) next b))))

;; Ex 1.32 a
(defn accumulate [combiner null-value term a next b]
  (loop [curr   a
         result null-value]
    (if (> curr b)
      result
      (recur (next curr)
             (combiner result
                       (term curr))))))

;; Ex 1.32 b
(defn acc-rec [combiner null-value term a next b]
  (if (> a b)
    null-value
    (combiner (accumulate combiner
                          null-value
                          term
                          (next a)
                          next
                          b)
              (term a))))

;; Ex 1.33
(defn filtered-accumulate [predicate combiner null-value term a next b]
  (loop [curr   a
         result null-value]
    (cond (> curr b) result
          (predicate curr) (recur (next curr)
                                  (combiner result
                                            (term curr)))
          :else            (recur (next curr) result))))

(defn sum-squares-of-primes [a b]
  (filtered-accumulate #(mr-prime? % 10)
                       +
                       0
                       #(* % %)
                       a
                       inc ; No even-odd trickery now
                       b))

(defn product-relatively-primes [n]
  (filtered-accumulate #(= (gcd % n) 1)
                       *
                       1
                       identity
                       1
                       inc
                       (dec n)))

;; Ex 1.37 a
(defn cont-frac [n d k]
  (accumulate (fn [result i] (/ (n i) (+ (d i) result)))
              (/ (n k) (d k))
              #(- k %)
              1
              inc
              (dec k)))

;; Ex 1.37 b
; Just be a smartass and define it in terms of acc-rec

;; Ex 1.38
(defn e-approx [n]
  (letfn [(d [i]
            (if (= (rem (dec i) 3) 1)
              (* 2 (inc (quot (dec i) 3)))
              1))]
    (cont-frac (constantly 1.0)
               d
               n)))

;; Ex 1.39
(defn tan-cf [x k]
  (letfn [(n [i]
            (if (= i 1) x (- (* x x))))
          (d [i]
            (dec (* 2 i)))]
    (cont-frac n d k)))

;; Ex 1.40
(defn cubic [a b c]
  (fn [x]
    (+ (* x x x)
       (* a (* x x))
       (* b x)
       c)))

;; Ex 1.41
(defn double [f]
  #(f (f %)))

;; Ex 1.42
(defn compose [f g]
  #(f (g %)))

;; Ex 1.43
(defn repeated [f n]
  (if (<= n 1)
    f
    (compose f (repeated f (dec n)))))
