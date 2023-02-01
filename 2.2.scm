;; Ex 2.17
;; ===============
(define (last-pair l)
  (cond ((null? (cdr l)) l)
        (else (last-pair (cdr l)))))

;; 2.18
;; ===============
(define (reverse l)
  (cond ((null? l) '())
        (else (append (reverse (cdr l)) (list (car l))))))

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


;; Ex 2.24
;; =============

;; Given: (list 1 (list 2 (list 3 4)))
;; Box-and-pointer structure

;; (1(2(3 4)     (2(3 4)         (3 4)           4
;;   |.|.|------> |.|.|--------> |.|.|--------> |.|/|
;;    |            |              |              |
;;    |            |              |              |
;;    v            v              v              v
;;    1            2              3              4

;; Tree structure
;;
;;                (1 (2 (3 4)))
;;                    /  \
;;                   /    \
;;                  /      \
;;                 1       (2 (3 4))
;;                            / \
;;                           /   \
;;                          /     \
;;                         2     (3 4)
;;                                / \
;;                               /   \
;;                              /     \
;;                             3       4  
;;


;; Ex 2.25
;; ================
;; Pick 7 using cars and cdrs
;;
;; x = (1 3 (5 7) 9)
;; y = ((7))
;; z = (1 (2 (3 (4 (5 (6 7))))))

;; (car (cdr (car (cdr (cdr x)))))
;; (car (car y))
;; (car (cdr (cdr (cdr (cdr (cdr (cdr z)))))))

;; Ex 2.27
;; ===============
(define (deep-reverse l)
  (cond ((null? l) '())
        ((not (pair? l)) l)
        (else (append (deep-reverse (cdr l))
                      (list (deep-reverse (car l)))))))


;; Ex 2.28
;; ===============
(define (fringe x)
  (cond ((null? x) '())
        ((not (pair? x)) (list x))
        (else (append (fringe (car x)) (fringe (cdr x))))))
