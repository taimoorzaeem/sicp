;; Ex 3.1
;; ==================

(define (accumulator init-sum)
  (lambda (sum)
    (begin (set! init-sum (+ init-sum sum))
           init-sum)))
