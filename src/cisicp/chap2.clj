(ns cisicp.chap2)

;; Ex 2.7

(defn make-interval [a b]
  [a b])

(defn lower-bound [[a b]]
  a)

(defn upper-bound [[a b]]
  b)

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defn div-interval [x y]
  {:pre [(or (> (lower-bound y) 0)
             (< (upper-bound y) 0))]}
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; Ex 2.8
(defn sub-interval [x y]
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))

;; Ex 2.9
(defn width-interval [x]
  (/ (- (upper-bound x) (lower-bound x)) 2))
