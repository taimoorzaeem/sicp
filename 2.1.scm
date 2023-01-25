;; Ex 2.1
;; ==================
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((or (and (< d 0) (not (< n 0))) 
               (and (< d 0) (< n 0)))
                  (cons (/ (* n -1) g) (/ (* d -1) g)))
          (else (cons (/ n g) (/ d g))))))


;; Ex 2.2
;; ==================
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define line (make-segment (make-point 10 20) 
                           (make-point 30 40)))

(define (midpoint-segment seg)
  (make-point (/ (+ (x-point (start-segment seg))
                    (x-point (end-segment seg))) 
                 2)
              (/ (+ (y-point (start-segment seg)) 
                    (y-point (end-segment seg))) 
                 2)))

;; Ex 2.3
;; ==================
;; skipped because easy


;; Ex 2.4
;; ==================

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))


;; Ex 2.5
;; =================
;; Express the definition of cons car cdr
;; for pairs of integers in terms of the 
;; expression 2^a * 3^b

(define (cons x y)
  (* (expt 2 x) (expt 3 y)))
(define (car z)
  (define (go num count)
    (cond ((= (remainder num 2) 0) (go (/ num 2) (+ count 1)))
          (else count)))
  (go z 0))
(define (cdr z)
  (define (go num count)
    (cond ((= (remainder num 3) 0) (go (/ num 3) (+ count 1)))
          (else count)))
  (go z 0))

