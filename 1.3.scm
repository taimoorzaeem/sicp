(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))


(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))


(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; ==============================
;; Ex 1.29
;; Simpson's Rule for integration
(define (odd? n)
    (= (remainder n 2) 1))
(define (even? n)
    (= (remainder n 2) 0))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (term-simp k)
    (* (cond ((odd? k) 4)
             ((or (= k 0) (= k n)) 1)
             ((even? k) 2))
       (y k)))
  (/ (* h (sum term-simp 0 inc n)) 3))


;; ============================
;; Ex 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


;; ===========================
;; Ex 1.31
;; Recursive Implementation
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;; Iterative Implementation
(define (prod-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (square x)
  (* x x))

(define (add-2 x)
  (+ x 2))

;; Assumption: User enters an even number
(define (pi-prod b)
  (define nu (* 2.0 (product square 4.0 add-2 (- b 2)) b))
  (define de (product square 3.0 add-2 (- b 1)))
  (/ nu de))

