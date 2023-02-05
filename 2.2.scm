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
;;       (1 (2 (3 4)))
;;           /  \
;;          /    \
;;         /      \
;;        1       (2 (3 4))
;;                   / \
;;                  /   \
;;                 /     \
;;                2     (3 4)
;;                       / \
;;                      /   \
;;                     /     \
;;                    3       4  
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


;; Ex 2.30
;; ==============
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
          (if (pair? sub-tree)
              (square-tree-map sub-tree)
              (* sub-tree sub-tree)))
       tree))

;; Ex 2.31
;; ==============
(define (tree-map f tree)
  (map (lambda (sub-tree)
          (if (pair? sub-tree)
              (tree-map f sub-tree)
              (f sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map (lambda (x) (* x x)) tree))

;; Ex 2.32
;; ==============
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (subset) 
                        (append (list (car s)) 
                                subset))
                      rest)))))

;;  Explanation of Ex 2.32:
;;  ===============
;;  The reason why this works is because
;;  for every item in the list, we just append
;;  the subsets of the rest of the list to the
;;  list containing the one item
;;
;;  for example:
;;  subsets (1) we append (()) to (list 1) and then 
;;      append it back to (()) = (() (1))
;;  subsets (1 2) we append (() (2)) to (list 1) and
;;      then append it back to (() (2) = (() (2) (1) (1 2))
;;  and so on ...
