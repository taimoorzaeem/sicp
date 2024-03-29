;; Ex 2.53
;; ===============

;; (a b c)
;; ((george))
;; ((y1 y2))
;; (y1 y2)
;; #f
;; #f
;; (red shoes blue socks)


;; Ex 2.54
;; ===============

(define (equal? a b)
  (cond ((and (pair? a) 
              (pair? b)) (and (eq? (car a) 
                                   (car b)) 
                              (equal? (cdr a) 
                                      (cdr b))))
        ((not (or (pair? a) 
                  (pair? b))) (eq? a b))
        (else #f)))



;; Ex 2.55
;; ===============

;; (car ''abracadabra)
;;
;; This is syntacticly equivalent to:
;;
;; (car (quote (quote abracadabra)))
;;
;; Hence, (quote (quote abracadabra)) would return the list
;; of symbols 'quote and 'abracadabra i.e. (quote abracadabra),
;; and so the car of that would take the first the list which is
;; the symbol quote.




;; The given differentiation code
;; ==============================
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) 
       (variable? v2) 
       (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))


(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
          (+ a1 a2))
        (else (list '+ a1 a2))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))


;; Partial Differentiation
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
          (make-sum
              (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (multiplicand exp)
                            (deriv (multiplier exp) var))))
        (else
          (error "unknown expression type: DERIV" exp))))

;; Ex 2.56
;; ==========================

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))
(define (exponent e) (caddr e))


(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
          (make-sum
              (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (multiplicand exp)
                            (deriv (multiplier exp) var))))
        ((exponentiation? exp)
          (make-product
            (make-product
                (exponent exp)
                (make-exponentiation (base exp)
                                    (make-sum (exponent exp) -1)))
            (deriv (base exp) var)))

        (else
          (error "unknown expression type: DERIV" exp))))

;; Ex 2.57
;; ==============

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))


;; Ex 2.58
;; =============
;;
;; SKIPPED




;; Ex 2.59
;; ===============

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? (car set) x) #t)
        (else (element-of-set? x (cdr set)))))


(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))


(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
          (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
          (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))


;; Ex 2.60
;; ==============

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))
