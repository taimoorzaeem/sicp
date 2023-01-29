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

;; 2.20
;; ===============
(define (same-parity . l)
  (define (go a l result)
    (if (null? l)
        result
        (go a 
            (cdr l) 
            (cond ((= (remainder (car l) 2) a) (append result 
                                                       (list (car l))))
                  (else result)))))
  (go (if (= (remainder (car l) 2) 0) 0 1)
      l 
      '()))
