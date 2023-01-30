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

;; Ex 2.21
;; ==============
(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items)
               (car items))
            (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

;; Ex 2.22
;; ==============
;; consing nil to a list stays in the list as an element

;; Ex 2.23
;; =============

;; Implementation of for-each

(define (for-each proc items)
  (define (for-each-iter proc items rv)
    (if (null? items)
        #t
        (for-each-iter proc (cdr items) (proc (car items)))))
  (for-each-iter proc items #t))
