;; Ex 3.1
;; ==================

(define (accumulator init-sum)
  (lambda (sum)
    (begin (set! init-sum (+ init-sum sum))
           init-sum)))

;; Ex 3.2
;; ==================

(define (make-monitored f)
  (let ((calls 0))
    (lambda (input)
      (if (eq? input 'how-many-calls)
          calls
          (begin (set! calls (+ calls 1))
                 (f input))))))
