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


;; Ex 2.33
;; ================
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;;(define (map p sequence)
;;  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;; Ex 2.34
;; ================
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff (* x higher-terms)))
              0 
              coefficient-sequence))


;; Ex 2.35
;; ===============
(define (count-leaves t)
  (accumulate (lambda (x y) (+ x y)) 
              0 
              (map (lambda (x) 
                     (if (pair? x)
                         (count-leaves x)
                         1)) 
                   t)))


;; Ex 2.36
;; ===============
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))



;; Ex 2.37
;; ===============
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))


;; Ex 2.38
;; ===============
(define fold-right accumulate)

(define (fold-left op init sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init sequence))

;; If the op is commutative then, the fold-left and fold-right 
;; will give the same answer for any sequence
;;
;; Example + is commutative but / is not commutative


;; Ex 2.39
;; ================
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))


;; Ex 2.40
;; ================

(define (enumerate-interval m n)
  (define (enumerate-interval-iter m n res)
    (if (> m n)
        res
        (cons m (enumerate-interval-iter (+ m 1) n res))))
  (enumerate-interval-iter m n '()))


(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

;; Simplified
(define (prime? n)
  (if (or (= n 2) (= n 3) (= n 5) (= n 7) (= n 11) (= n 13))
      #t
      #f))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))


(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap (lambda (i) 
                   (map (lambda (j) (list i j)) 
                        (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))



;; Ex 2.41
;; =================

(define (ordered-triples n s)
  (filter (lambda (p) (= (+ (car p) (cadr p) (caddr p)) s)) 
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k) (list i j k))
                                     (enumerate-interval 1 (- j 1))))
                              (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))


;; Ex 2.42
;; ================

(define (queens board-size)

  (define empty-board '())

  (define (adjoin-position row col rest)
    (append rest (list (cons row col))))

  (define (safe? k positions)
    (define (attacks? q1 q2)
      (or (= (car q1) (car q2)) ;; check row
          (= (cdr q1) (cdr q2)) ;; check column
          (= (abs (- (car q1) (car q2))) ;; check diagonal
             (abs (- (cdr q1) (cdr q2))))))

    (define (iter q rest)
      (or (null? rest)
          (and (not (attacks? q (car rest)))
               (iter q (cdr rest)))))


    (let ((kth-queen (list-ref positions (- k 1)))
          (other-queens (filter (lambda (q)
                                  (not (= k (cdr q))))
                                positions)))

    (iter kth-queen other-queens)))


  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position
                      new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))


;; Ex 2.43
;; =================

;; In this case the procedure will take exponential time
;; so the program will finish in T^board-size



;; Ex 2.44
;; ================

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below (beside smaller smaller) painter))))

