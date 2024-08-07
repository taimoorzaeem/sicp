;; Section 4.1

(define (eval exp env)
	(cond ((self-evaluating? exp) exp)
				((varianle? exp) (lookup-variable-value exp env))
				((quoted? exp) (text-of-quotation exp))
				((assignment? exp) (eval-assignment exp env))
				((definition? exp) (eval-definition exp env))
				((if? exp) (eval-if exp env))
				((lambda? exp) (make-procedure (lambda-parameters exp)
																			 (lambda-body exp)
																			 env))
				((begin? exp)
			   (eval-sequence (begin-actions exp) env))
				((cond? exp) (eval (cond->if exp) env))
				((application? exp)
				 (apply (eval (operator exp) env)
								(list-of-values	(operands exp) env)))
				(else
					(error "Unknown expression type: EVAL" exp))))


(define	(apply procedure arguments)
	(cond ((primitive-procedure? procedure)
				 (apply-primitive-procedure procedure arguments))
				((compound-procedure? procedure)
				 (eval-sequence
					  (procedure-body procedure)
						(extend-environment
							(procedure-body procedure)
						  arguments
							(procedure-environment procedure))))
				(else (error "Unknown procedure type: APPLY" procedure))))


(define (list-of-values exps env)
	(if (no-operands? exps)
			'()
			(cons (eval (first-operand exps) env)
						(list-of-values (rest-operands exps) env))))


(define (eval-if exp env)
	(if (true? (eval (if-predicate exp) env))
			(eval (if-consequent exp) env)
			(eval (if-alternative exp) env)))


(define (eval-sequence exps env)
	(cond ((last-exp? exps)
				 (eval (first-exp exps) env))
				(else
					(eval (first-exp exps) env)
					(eval-sequence (rest-exps exp) env))))


(define (eval-assignment exp env)
	(set-variable-value! (assignment-variable exp)
											 (eval (assignment-value exp) env)
											 env)
	'ok)


(define (eval-definition exp env)
	(define-variable! (definition-variable exp)
										(eval (definition-value exp) env)
										env)
	'ok)

;; Ex 4.1
;; ================

(define (list-of-values-l2r exps env)
	(if (no-operands? exps)
			'()
			(let ((first-exp (eval (first-operand exps) env))
					(cons first-exp
								(list-of-values-l2r (rest-operands exps) env))))))


(define (list-of-values-r2l exps env)
		(list-of-values-l2r (reverse exps) env))

;; Representing Expressions

(define (self-evaluating exp)
	(cond ((number? exp) true)
				((string? exp) true)
				(else false)))


(define (variable? exp) symbol? exp)

(define (quoted exp) (tagged-list? exp) ('quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
	(if (pair? exp)
			(eq? (car exp) tag)
			false))

(define (assignment? exp) (tagged-list? exp 'set))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))


(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
	(if (symbol? (cadr exp))
			(cadr exp)
			(caadr exp)))
(define (definition-value exp)
	(if (symbol? (cadr exp))
			(caddr exp)
			(make-lambda (cdadr exp)   ; formal parameters
									 (cddr exp)))) ; body of procedure


(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
	(cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
	(if (not (null? (cdddr exp)))
			(cadddr exp)
			'false))

(define (make-if predicate consequent alternative)
	(list 'if predicate consequent alternative))

(define (begin? exp 
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))a

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))


(define (sequence->exp seq)
	(cond ((null? seq) seq)
				((last-exp? seq) (first-exp seq))
				(else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops)


(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-classses exp) (cdr exp))
(define (cond-else-clause? clause)
	(eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
	(if (null? clauses)
			'false        ;no else clause
			(let ((first (car clause))
					 (rest (cdr clause)))
				(if (cond-else-clause? first)
						(if (null? rest)
								(sequence->exp (cond-actions first))
								(error "ELSE clause isn't last: COND->IF" clauses))
						(make-if (cond-predicate first)
										 (sequence->exp (cond-actions first))
										 (expand-clauses rest))))))

;; Ex 4.2
;; ==================
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))


;; Ex 4.4
;; ==================

(define (eval exp env)
	(cond ((self-evaluating? exp) exp)
				((varianle? exp) (lookup-variable-value exp env))
				((quoted? exp) (text-of-quotation exp))
				((assignment? exp) (eval-assignment exp env))
				((definition? exp) (eval-definition exp env))
				((and? exp) (eval-and (cdr exp) env))
				((or? exp) (eval-or (cdr exp) env))
				((if? exp) (eval-if exp env))
				((lambda? exp) (make-procedure (lambda-parameters exp)
																			 (lambda-body exp)
																			 env))
				((begin? exp)
			   (eval-sequence (begin-actions exp) env))
				((cond? exp) (eval (cond->if exp) env))
				((application? exp)
				 (apply (eval (operator exp) env)
								(list-of-values	(operands exp) env)))
				(else
					(error "Unknown expression type: EVAL" exp))))


(define (eval-and exp env)
	(if (eq? (eval (car exp) env) #f)
			#f
			(eval-and (cdr exp) env)))

(define (eval-or exp env)
	(if (eq? (eval (car exp) env) #t)
			#t
			(eval-or (cdr exp) env)))


;; Ex 4.6
;; ================

(define (let-vars exp) (map car (cadr exp)))
(define (let-exps exp) (map cadr (cadr exp)))
(define (let-body exp) (cddr exp))

(define (let->combination exp)
	(cons (make-lambda (let-vars exp) 
										 (let-body exp)) 
				(let-exp exp)))




(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))


define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())



;; Ex 4.11
;; =================

(define (make-frame variables values)
  (map cons variables values))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))
(define (add-binding-to-frame var val frame)
  (cons (cons var val) frame))


;; Ex 4.15
;; ================

(define (run-forever) (run-forever))
(define (try p)
  (if (halts? p p) (run-forever) 'halted))

;; It is impossible to write a procedure "halts?"
;; because say if the machine halts it runs forever
;; and if it doesn't halt it's halted. This is a 
;; contradiction.


;; Ex 4.21
;; ===============a

;; a)

((lambda (n)
    ((lambda (fact) (fact fact n))
     (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
  10)

((lambda (n)
    ((lambda (fibo) (fibo fibo n))
     (lambda (fib k)
        (cond ((= k 0) 1)
              ((= k 1) 1)
              (else (+ (fib fib (- k 2)) (fib fib (- k 1))))))))
  10)

;; b)

(define (f x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
      (if (= n 0) true (od? ev? od? (- n 1) )))
   (lambda (ev? od? n)
      (if (= n 0) false (ev? ev? od? (- n 1))))))
