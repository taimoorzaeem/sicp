;; Section 3.5
;; ================
;; STREAMS

(define the-empty-stream '())
(define (cons-strean a b) (cons a (delay b)))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))


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
