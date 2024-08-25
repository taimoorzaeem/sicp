;; SECTION 5.1
;; =============


;; Ex 5.2
;; ==========

;; (controller
;;    (assign product (const 1))
;;    (assign counter (const 1))
;;  test-counter
;;    (test (op >) (reg counter) (const n))
;;    (branch (label factorial-done))
;;    (assign t1 (op *) (reg product) (reg counter))
;;    (assign t2 (op +) (reg counter) (const 1))
;;    (assign product (reg t1))
;;    (assign counter (reg t2))
;;    (goto (label test-counter))
;;  factorial-done)


;; Ex 5.3
;; ==========
 
;; (controller
;;   (assign x (op read))
;;   (assign guess (const 1.0))
;;  test-good
;;   (test (op good-enough?) (reg guess) (reg x))
;;   (branch (label sqrt-done))
;;   (assign t (op improve) (reg guess) (reg x))
;;   (assign guess (reg t))
;;   (goto (label test-good))
;;  sqrt-done)
