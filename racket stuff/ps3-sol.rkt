;Problem 1
;1a
(define (harmonic n)
  (if (= n 0) 0
      (+ (/ 1 n) (harmonic (- n 1)))))
;1b
(define (Eulerest n)
  (if (= n 0) 0      
      (abs (- (harmonic n) (log n)))))
;Problem 2
(define (prime? n)
  (define (divisor? k) (= 0 (modulo n k)))
  (define (divisors-upto k)
    (and (> k 1)
         (or (divisor? k) (divisors-upto (- k 1)))))
  (not (divisors-upto (- n 1))))
; count primes function
(define (count-primes m)
  (if (= m 1) 0
      (if (prime? m)
      (+ 1 (count-primes (- m 1)))
      (count-primes (- m 1)))))
;Problem 3
(define (rel-prime a b)
  (define (divides-both d)
    (and (= 0 (modulo a d))
         (= 0 (modulo b d))))
  (define (divisor-upto k)
    (and (> k 1)
         (or (divides-both k)
             (divisor-upto (- k 1)))))
  (not (divisor-upto (min a b))))
;count rel prime function
;N O T  D O N E!!!!!
(define (count-rel-prime n)
  (define (rel-prime-range a b)
  (if (= a 1) 1
      (if (equal? (rel-prime a b) #t)
          (+ 1 (rel-prime-range (- a 1) b))
          (rel-prime-range (- a 1) b))))
  (define (count-help n)
    (if (= n 1) 1
        (+ (rel-prime-range (- n 1) n) (count-help (- n 1)))))
  (count-help n))


;Problem 4
;4a
(define (lucas n)
  (cond ((= n 0) 2)
      ((= n 1) 1)
          ((> n 1) (+ (lucas (- n 1)) (lucas (- n 2))))))
;4b
(define (Lucas-ratio n)
  (/ (lucas n) (lucas (- n 1))))

;to define fib
(define (fib n)
  (if (= n 0) 0
      (if (= n 1) 1
          (+ (fib (- n 1)) (fib (- n 2))))))

(define (Fibonacci-ratio n)
  (/ n (- n 1)))
;4c
;(lucas 30)
;(lucas 35)
;(lucas 40)

(define (fast-Lucas-help n k luc-a luc-b)
  (if (= n k)
      luc-a
      (fast-Lucas-help n (+ k 1) (+ luc-a luc-b) luc-a)))
(define (rec-call-fast-Lucas k)
  (if (= k 1) 0
      (+ 1 (fast-Lucas-help (- k 1) 1 1 2))))


(define (rec-call-Lucas k)
  (cond ((= k 1) 0)
        ((= k 2) 2)
        ((= k 3) 4)
        ((= k 4) 8)
        ((= k 5) 14)
        ((= k 6) 24)))
(define (rec-call-fast-lucas-helper k)
  (if (= k 1) 0
        (+ 1 (rec-call-fast-lucas-helper (- k 1)))))   

;to solve lucas numbers faster
(define (fast-Lucas n) (fast-Lucas-help n 1 1 2))
;(fast-Lucas 50)

;Problem 5
;5a
(define (golden n)
  (if (= n 1) 2
      (+ 1 (/ 1 (golden (- n 1))))))
;5b
(define (golden-sqrt n)
  (if (= n 0) 1
      (sqrt(+ 1 (golden-sqrt (- n 1))))))
;6

(define (explain-interval-sum)
  (define a "The code doesn't account for all pairs and works only for even differences of pairs and only works if m is smaller than n")
  (define b "you would also need to make the code execute if m>n")
  (define c "if the numbers are odd the code would loading infinitely")
  (define d "idk")
  b)
(explain-interval-sum)
;7
(define (ack m n)
  (if (= m 0) (+ 1 n)
      (if (and (> m 0) (= n 0)) (ack (- m 1) 1)
          (if (and (> m 0) (> n 0)) (ack (- m 1) (ack m (- n 1) )) ))))
;8
(define (factorial n)
  (if (= n 0) 1
      (* (factorial (- n 1)) n)))
(define (catalan n)
  (if (= n 0) 1
      (/ (factorial (* 2 n)) (* (factorial(+ n 1)) (factorial n)))))

;9
(define pi-approx
  (let ((a (/ (+ 23 (* 4 (sqrt 34))) 2))
      (b (/ (+ (* 19 (sqrt 2)) (* 7 (sqrt 17))) 2))
       (c (+ 429 (* 304 (sqrt 2))))
        (d (/ (+ 627 (* 442 (sqrt 2))) 2)))
(let ((u (* (expt (+ a (sqrt (- (expt a 2) 1))) 2) (expt (+ b (sqrt (- (expt b 2) 1))) 2) (+ c (sqrt (- (expt c 2) 1))) (+ d (sqrt (- (expt d 2) 1))))))

  (/ (log (+ (expt (* 2 u) 6) 24)) (sqrt 3502)))))

;Problem 10
(define (gauss-legendre tol)
  (define (help a b t p)
    (let ((a (/ (+ a b) 2))
          (b (sqrt(* a b)))
          (t (-  t (* p (expt (- a (/ (+ a b) 2)) 2))))
          (p (* 2 p)))
    (if (< (abs (- a b)) tol)
    (/ (expt (+ a b) 2) (* 4 t))
    (help a b t p))))
  (help 1 (/ 1 (sqrt 2)) (/ 1 4) 1))


  
          
  

    



  

