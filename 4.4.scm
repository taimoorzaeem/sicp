;; SECTION 4.4
;; ================
;; LOGIC PROGRAMMING


;; Ex 4.55
;; =============

;; Queries:
;; =======
;; (supervisor ?name (Bitdiddle Ben))
;; (job ?name (accounting . ?type))
;; (address ?name (Slumerville . ?street-address))



;; Ex 4.56
;; =============

;; Queries:
;; =======
;; (and (supervisor ?name (Bitdiddle Ben))
;;      (address ?name ?address))
;; (and (salary (Bitdiddle Ben) ?ben-salary)
;;      (salary ?name ?salary)
;;      (lisp-value < ?salary ?ben-salary))
;; (and (supervisor ?name ?supervisor)
;;      (job ?supervisor ?supervisor-job)
;;      (not (job ?supervisor (computer . ?title)))))
