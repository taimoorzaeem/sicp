(define (abs x)
  (if (< x 0)
      (- x)
      x))

; ===================
; Ex 1.2

(/ (+ 5 
      4 
      (- 2 
         (- 3 
            (+ 6 
               (/ 4 5))))) 
   (* 3
      (- 6 2)
      (- 2 7)))

; Ex 1.3

(define (f x y z)
  (cond ((and (< x y) (< x z)) (+ (* y y) (* z z)))
        ((and (< y x) (< y z)) (+ (* x x) (* z z)))
        ((and (< z x) (< z y)) (+ (* x x) (* y y)))))

; Ex 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; Ex 1.5

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

; ============================
; Newton's Square Root Method
; ============================
(define (square x)
  (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))


(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? previous-guess guess)
  (< (abs (/ (- guess previous-guess) guess)) 0.00000000001)) 

(define (sqrt x)
  (sqrt-iter 1.0 x))

; Ex 1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter1 guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter1 (improve guess x) x)))



;; Ex 1.8

(define (cube-root x)
  (cube-root-iter 1.0 x))

(define (cube-root-iter guess x)
  (if (good-enough? guess (improve-c guess x))
      guess
      (cube-root-iter (improve-c guess x) x)))

(define (improve-c guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
