;; Section 4.2
;; =============


;; Ex 4.25
;; ===========

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

;; Output:
;;
;; We get a maximum recursion depth reached error, because the expression
;; (* n (factorial (- n 1))) keeps evaluating forever.
;; In a normal-order-language our definition will work, because the argument
;; will be passed to unless without being evaluated. They will only be
;; evaluated after calling the function `unless`.
