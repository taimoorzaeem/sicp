;; SECTION 4.3


;; Ex 4.35
;; =============

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))
