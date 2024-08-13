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

