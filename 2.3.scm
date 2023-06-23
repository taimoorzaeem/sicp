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
