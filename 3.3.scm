(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
	(set-car! queue item))
(define (set-rear-ptr! queue item)
	(set-cdr! queue item))

(define (empty-queue? queue)
	(null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
	(if (empty-queue? queue)
			(error "FRONT called with an empty queue" queue)
			(car (front-ptr queue))))

(define (insert-queue! queue item)
	(let ((new-pair (cons item '())))
		(cond ((empty-queue? queue)
				(set-front-ptr! queue new-pair)
				(set-rear-ptr! queue new-pair)
				queue)
				(else
					(set-cdr! (rear-ptr queue) new-pair)
					(set-rear-ptr! queue new-pair)
					queue))))

(define (delete-queue! queue)
	(cond ((empty-queue? queue)
				 (error "DELETE! called with an empty queue" queue))
				(else (set-front-ptr! queue (cdr (front-ptr queue)))
							queue)))

;; Ex 3.21
;; ================
(define (print-queue queue)
	(car queue))

;; Ex 3.22
;; ================
(define (make-queue) 
   (let ((front-ptr '()) 
         (rear-ptr '())) 
     (define (set-front-ptr! item) 
       (set! front-ptr item)) 
     (define (set-rear-ptr! item) 
       (set! rear-ptr item)) 
     (define (dispatch m) 
       (cond ((eq? m 'front-ptr) front-ptr) 
             ((eq? m 'rear-ptr) rear-ptr) 
             ((eq? m 'set-front-ptr!) set-front-ptr!) 
             ((eq? m 'set-rear-ptr!) set-rear-ptr!) 
             (else 
              (error "Undefined operation: QUEUE" m)))) 
     dispatch))
