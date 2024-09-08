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


;; Ex 4.58
;; ==============

;; Queries:
;; =======
;; (rule (big-shot ?person ?division)
;;    (and (job ?name (?division . ?title))
;;         (supervisor ?name ?boss)
;;         (job ?boss (?division-2 . ?title-2))
;;         (not (same ?division ?division-2))))


;; Ex 4.59
;; ==============

;; Queries:
;; =======
;; (meeting ?where (Friday ?time))
;;
;; (rule (meeting-time ?person ?day-and-time)
;;       (or (and (job ?person (?division . ?title))
;;                (meeting ?division ?day-and-time))
;;           (meeting whole-company ?day-and-time)))
;;
;; (meeting-time (Hacker Alyssa P) (Wednesday ?time))


;; Ex 4.61
;; =============

;; Queries:
;; =======
;; (?x next-to ?y in (1 (2 3) 4))
;; ==>  ((2 3) next-to 4 in (1 (2 3) 4))
;;      (1 next-to (2 3) in (1 (2 3) 4))
 
;; (?x next-to 1 in (2 1 3 1))
;; ==> (3 next-to 1 in (2 1 3 1))
;;     (2 next-to 1 in (2 1 3 1))


;; Ex 4.62
;; ============

;; Queries:
;; =======
;; (rule (last-pair (?x) (?x)))
;; (rule (last-pair (?x . ?y) (?z))
;;       (last-pair (?y) (?z)))


;; Ex 4.63
;; ============

;; Queries:
;; =======
;; (rule (grandson ?x ?y)
;;       (and (son ?x ?z)
;;            (son ?z ?y)))


;; Ex 4.64
;; ============

;; Answer:
;;   The query (outranked-by (Bitdiddle Ben) ?who) computes
;; (outranked-by ?middle-manager ?who) which performs the same
;; operation again and again and this driving the system into
;; an infinite loop.


;; Ex 4.68
;; ===========

;; (rule (reverse () ()))
;; (rule (reverse (?h . ?t) ?y)
;;       (and (reverse ?t ?reversed-t)
;;            (append-to-form ?reversed-t (?h) ?y)))



;; Driver Loop Code

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")


(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
              (instantiate
              q
              frame
              (lambda (v f)
                (contract-question-mark v))))
            (qeval q (singleton-stream '()))))
          (query-driver-loop)))))


(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))


(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))


(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (stream-append-delayed
       (find-assertions query-pattern frame)
       (delay (apply-rules query-pattern frame))))
    frame-stream))


(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts) frame-stream))))


(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts) frame-stream)))))


(define (negate operands frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if (stream-null? 
           (qeval (negated-query operands) 
                  (singleton-stream frame)))
          (singleton-stream frame)
           the-empty-stream))
    frame-stream))
