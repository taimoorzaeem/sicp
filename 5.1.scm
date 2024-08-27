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


;; Ex 5.4
;; ==========

;; a)

;; (controller
;;   (assign continue (label expt-done))
;;  expt-loop
;;   (test (op =) (reg n) (const 0))
;;   (branch (label base-case))
;;   (save continue)
;;   (assign n (op -) (reg n) (const 1))
;;   (assign continue (label after-expt))
;;   (goto (label expt-loop))
;;  after-expt
;;   (restore continue)
;;   (assign val (op *) (reg n) (reg val))
;;   (goto (reg continue))
;;  base-case
;;   (assign val (const 1))
;;   (goto (reg continue))
;;  expt-done)

;; b)

;; (controller
;;   (assign val (const 1))
;;  expt-loop
;;   (test (op =) (reg n) (const 0))
;;   (branch (label expt-done))
;;   (assign val (op *) (reg val) (reg b))
;;   (assign n (op -) (reg n) (const 1))
;;   (goto (label expt-loop))
;;  expt-done)


;; Ex 5.6
;; ===========

;; (restore continue)
;; (assign n (op -) (reg n) (const 2))
;; (save continue)

;; In the instructions above the instructions restore and save are extra
;; and hence should be removed.
