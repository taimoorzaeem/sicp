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


;; Ex 4.57
;; =============

;; (rule (can-replace ?person1 ?person2) 
;;   (and (job ?person1 ?job1) 
;;        (or (job ?person2 ?job1) 
;;            (and (job ?person2 ?job2) 
;;                 (can-do-job ?job1 ?job2))) 
;;        (not (same ?person1 ?person2))))
;;
;; Queries:
;; =======
;; (can-replace ?x (Fect Cy D))
;;
;; (and (can-replace ?a ?b)
;;      (salary ?a ?as)
;;      (salary ?b ?bs)
;;      (lisp-value < ?as ?bs))
