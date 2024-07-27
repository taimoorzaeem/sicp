;; Section 4.1

(define (eval exp env)
	(cond ((self-evaluating? exp) exp)
				((varianle? exp) (lookup-variable-value exp env))
				((quoted? exp) (text-of-quotation exp))
				((assignment? exp) (eval-assignment exp env))
				((definition? exo) (eval-definition exp env))
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





