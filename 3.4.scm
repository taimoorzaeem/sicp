;; Ex 3.38
;; ==============

;; All possible values
;; 
;; +10=110  -20=90  /2=45
;; +10=110  /2=55   -20=35
;; -20=90   /2=45   +10=55
;; -20=80   +10=90  +10=45
;;  /2=50   -20=30  +10=40
;;  /2=50   +10=60  -20=40

;; Ex 3.40
;; ==============

;; Possiblities remain after serializing
;;
;; P1 = 100 or P2 = 1000

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
	(if (car cell) true (begin (set-car! cell true) false)))

(define (make-mutex)
	(let ((cell (list false)))
		(define (the-mutex m)
			(cond ((eq? m 'acquire)
						 (if (test-and-set! cell)
								 (the-mutex 'acquire)))
						((eq? m 'release) (clear! cell))))
		the-mutex))


(define (make-serializer)
	(let ((mutex (make-mutex)))
		(lambda (p)
			(define (serialized-p . args)
				(mutex 'acquire)
				(let ((val (apply p args)))
					(mutex 'release)
					val))
			serialized-p)))


;; Ex 3.47
;; ================

;; Implement semaphore using mutex
(define (make-semaphore)
	(let ((mutex (make-mutex))
				(c 0))
		(define (acquire)
			(mutex 'acquire)
			(cond ((< c n) (set! c (+ c 1)) (mutex 'release))
						(else (mutex 'release) (acquire))))
		(define (release)
			(mutex 'acquire)
      (if (<= 1 c) (set! c (- c 1))
									 (error "This semaphore is not acquired yet"))
      (mutex 'release))
    (define (dispatch m)
      (cond [(eq? m 'acquire) (acquire)]
            [(eq? m 'release) (release)]
            [else (error "Unknown message sent to a semaphore" m)]))
    dispatch))
