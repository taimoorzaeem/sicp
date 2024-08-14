;; Section 3.5
;; ================
;; STREAMS

(define the-empty-stream '())
(define (cons-strean a b) (cons a (delay b)))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-null? s) (equal? s the-empty-stream))


;; Ex 3.50
;; =============

(define (stream-map proc . argstreams)
	(if (stream-null? (car argstreams))
			the-empty-stream
			(cons-stream
			 (apply proc (map stream-car argstreams))
			 (apply stream-map
						  (cons proc (map stream-cdr argstreams))))))


;; Ex 3.51
;; =============

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))

(define (display-line x) (newline) (display x))
(define (show x) (display-line x) x)

(define x
  (stream-map show 
              (stream-enumerate-interval 0 10)))


;; 3.52
;; ============

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s)) 

(define sum 0)

(define (accum x) (set! sum (+ x sum)) sum)

(define seq 
  (stream-map accum 
              (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))

(define z 
  (stream-filter (lambda (x) (= (remainder x 5) 0)) 
                 seq))

;; Output
;;
;; (stream-ref y 7) ==> 136
;;
;; (display-stream z)
;; ==>
;;  10
;;  15
;;  45
;;  55
;;  105
;;  120
;;  190
;;  210


;; Ex 3.53
;; =============

(define (add-streams s1 s2) (stream-map + s1 s2))

(define s (cons-stream 1 (add-streams s s)))

;; Output
;; ======
;;
;; s == (1 2 4 8 16 32 ...)


;; Ex 3.54
;; ============

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (mul-streams s1 s2) (stream-map * s1 s2))

(define factorials
  (cons-stream 1 
               (mul-streams (stream-cdr integers) 
                            factorials)))

;; Output
;;
;; (stream-ref factorials 0) ==> 1
;; (stream-ref factorials 2) ==> 6
;; (stream-ref factorials 3) ==> 24
;; (stream-ref factorials 5) ==> 720


;; Ex 3.55
;; ===========

(define (partial-sums s)
  (cons-stream (stream-car s) 
               (add-streams (stream-cdr s) 
                            (partial-sums s))))


;; Output
;;
;; (stream-ref (partial-sums integers) 0) ==> 1
;; (stream-ref (partial-sums integers) 1) ==> 3
;; (stream-ref (partial-sums integers) 2) ==> 6
;; (stream-ref (partial-sums integers) 3) ==> 10


;; Ex 3.56
;; =============

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
        (let ((s1car (stream-car s1))
              (s2car (stream-car s2)))
          (cond ((< s1car s2car)
                 (cons-stream
                  s1car
                  (merge (stream-cdr s1) s2)))
                ((> s1car s2car)
                 (cons-stream
                  s2car
                  (merge s1 (stream-cdr s2))))
                (else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))))))))

(define S 
  (cons-stream 1 
               (merge (scale-stream S 2)
                      (merge (scale-stream S 3) 
                             (scale-stream S 5)))))

;; Ex 3.58
;; ============

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;; Interpretation:
;;
;; The result is the floating-point representation of (/ num den) with
;; radix as the base.
;; (/ 1.0 7) ==> .142857142857...
