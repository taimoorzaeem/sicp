;; Ex 2.17
;; ===============
(define (last-pair l)
  (cond ((null? (cdr l)) l)
        (else (last-pair (cdr l)))))

;; 2.18
;; ===============
(define (reverse l)
  (cond ((null? l) '())
        ((null? (cdr l)) (car l))
        (else (cons (reverse (cdr l)) (car l)))))
