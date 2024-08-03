;; Section 3.5
;; ================
;; STREAMS


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
