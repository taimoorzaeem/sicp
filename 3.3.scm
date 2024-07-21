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

;; Ex 3.24
;; ================
(define (make-table-with-key-proc same-key?)
	(let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
		(define (lookup key-1 key-2)
			(let ((subtable
							(assoc key-1 (cdr local-table))))
				(if subtable
						(let ((record
									 (assoc key-2 (cdr subtable))))
							(if record (cdr record) false))
						false)))
		(define (insert! key-1 key-2 value)
			(let ((subtable
							(assoc key-1 (cdr local-table))))
				(if subtable
						(let ((record
						       (assoc key-2 (cdr subtable))))
			        (if record
				          (set-cdr! record value)
				          (set-cdr! subtable
			                      (cons (cons key-2 value)
			                            (cdr subtable)))))
			      (set-cdr! local-table
			                (cons (list key-1 (cons key-2 value))
		                        (cdr local-table)))))
		  'ok)
		(define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))


;; Ex 3.27
;; ==================

;; No, the scheme would not have worked if we have simply defined the function
;; as (memoize fib). This is because fib does not recursively call to memoize
;; the result.

(define (logical-not s)
	(cond ((= s 0) 1)
				((= s 1) 0)
				(else (error "Invalid signal" s))))

(define (inverter input output)
	(define (invert-input)
		(let ((new-value (logical-not (get-signal input))))
			(after-delay inverter-delay
									(lambda () (set-signal! output new-value)))))
	(add-action! input invert-input) 'ok)

(define (logical-and a b)
	(cond ((and (= a 0) (= b 0)) 0)
				((and (= a 0) (= b 1)) 0)
				((and (= a 1) (= b 0)) 0)
				((and (= a 1) (= b 1)) 1)
				(else (error "Invalid signals" a b))))

(define (and-gate a1 a2 output)
	(define (and-action-procedure)
		(let ((new-value
					(logical-and (get-signal a1) (get-signal a2))))
		 (after-delay
			and-gate-delay
			(lambda () (set-signal! output new-value)))))
	(add-action! a1 and-action-procedure)
	(add-action! a2 and-action-procedure)
	'ok)

;; Ex 3.28
;; ==============

(define (logical-or a b)
	(cond ((and (= a 0) (= b 0)) 0)
				((and (= a 0) (= b 1)) 1)
				((and (= a 1) (= b 0)) 1)
				((and (= a 1) (= b 1)) 1)
				(else (error "Invalid signals" a b))))

(define (or-gate a1 a2 output)
	(define (or-action-procedure)
		(let ((new-value
					(logical-or (get-signal a1) (get-signal a2))))
		 (after-delay or-gate-delay
									(lambda () (set-signal! output new-value)))))
	(add-action! a1 or-action-procedure)
	(add-action! a2 or-action-procedure)
	'ok)

;; Ex 3.29
;; ==============

(define (or-gate-other a1 a2 output)
	(define (or-action-procedure)
		(let ((new-value
					(logical-not (logical-and	(logical-not (get-signal a1))
															      (logical-not (get-signal a2))))))
			(after-delay (+ (* inverter-delay 2) and-gate-delay)
									 (lambda () (set-signal! output new-value)))))
	(add-action! a1 or-action-procedure)
	(add-action! a2 or-action-procedure)
	'ok)
