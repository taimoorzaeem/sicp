(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

;; Ex 1.9

;(define (+ a b)
;  (if (= a 0) b (inc (+ (dec a) b))))

;(+ 3 2)

;; (+ 3 2)
;; (inc (+ 2 2))
;; (inc (inc (+ 1 2)))
;; (inc (inc (inc (+ 0 2))))
;; (inc (inc (inc 2)))
;; (inc (inc 3))
;; (inc 4)
;; 5 == (+ 3 2)
;; This is NOT tail-recursive, it is a recursive process

;(define (+ a b)
;  (if (= a 0) b (+ (dec a) (inc b))))

;(+ 3 2)

;; (+ 3 2)
;; (+ 2 3)
;; (+ 1 4)
;; (+ 0 5)
;; 5 == (+ 3 2)
;; Yes, this is tail-recursive, it is an iterative process

;; ======================
;; Ex 1.10
;; Ackermann's function

(define (A x y)
  (cond ((= y 0) 0)
      ((= x 0) (* 2 y))
      ((= y 1) 2)
      (else (A (- x 1) (A x (- y 1))))))

; (A 3 3)

;; (A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))


;; ======================
;; Ex 1.11

;; Recursive Process
(define (f n)
    (if (< n 3)
        n
        (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))


;; Iterative Process
(define (ff n)
    (if (< n 3)
        n
        (ff-iter 4 2 1 n)))

(define (ff-iter a b c n)
    (if (= n 3)
        a 
        (ff-iter (+ a (* 2 b) (* 3 c)) a b (- n 1))))

;; Ex 1.16
;; ==================================

(define (even? n)
    (= (remainder n 2) 0))

(define (expn b n)
    (expn-iter 1 b n))

(define (expn-iter a b n)
    (cond ((= n 1) a)
          ((even? n) (expn-iter (* a (* b b)) b (/ n 2)))
          (else (expn-iter (* a b) b (- n 1)))))


;; Ex 1.17
;; ==================================

(define (halve n)
    (/ n 2))

(define (double n)
    (* n 2))

(define (mult a b)
    (cond ((< b 1) 0)
          ((= b 1) a)
          ((even? b) (mult (double a) (halve b)))
          (else (+ a (mult a (- b 1))))))



;;Ex 1.18
;; ===================================

(define (mul a b)
    (mul-iter 0 a b))

(define (mul-iter acc a b)
    (cond ((= b 1) acc)
          ((even? b) (mul-iter (+ acc (double a)) a (halve b)))
          (else (mul-iter (+ a acc) a (- b 1)))))

;; Ex 1.19
;; ==================================
;; Fibonacci numbers in Log(n)

(define (fib n)
    (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count) (fib-iter a
                                   b
                                   (+ (* p p) (* q q))
                                   (+ (* 2 p q) (* q q))
                                   (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))

;; ===================================
;; Greatest common divisor using Euclid's Method

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Ex 1.20


;; Normal Order Evaluation
;; (gcd 206 40)
;; (gcd 40 (remainder 206 40))
;; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;; (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;; ...
;; ...

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
  ((divides? test-divisor n) test-divisor)
  (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder 
            (square (expmod base (/ exp 2) m))
            m))
        (else
          (remainder
            (* base (expmod base (- exp 1) m))
            m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; Ex 1.22
;; ==================================

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes a b)
  (cond ((= a b) (timed-prime-test a))
        (else (timed-prime-test a)
              (search-for-primes (+ a 2) b))))
