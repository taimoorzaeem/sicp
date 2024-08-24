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
