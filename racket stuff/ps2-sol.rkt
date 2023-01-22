;Problem 1 - Matthew Ghanie
;1a
(define (fizzbuzz x)
  (if (and (=(modulo x 3) 0) (=(modulo x 5) 0)) "fizzbuzz"
  (if (=(modulo x 3) 0) "fizz"
  (if (=(modulo x 5) 0) "buzz"
      x))))

;1b
(define (fizz x) (if (=(modulo x 3) 0) "fizz"))
(define (buzz x) (if (=(modulo x 5) 0) "buzz"))
(define (fizzbuzz2 x)
  (if (and (equal? (fizz x) "fizz") (equal? (buzz x) "buzz"))
      (string-append (fizz x) (buzz x))
      (if (equal? (fizz x) "fizz")
          (fizz x)
          (if (equal? (buzz x) "buzz")
          (buzz x)
          x))))

;Problem 2
(define (piecewise x)
  (define pi 3.142)
  (if (> x (* 2 pi)) (- x (* 2 pi))
      (if (and (>= x (- 0 pi)) (<= x (* 2 pi))) (sin x)
          (if (< x (- 0 pi)) (- (- 0 x) pi)))))
;Problem 3
(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(define (add n m)
  (if (= n 0) m
      (add (dec n) (inc m) )))

;Problem 4
(define (mult n m)
  (if (or (= n 0) (= m 0)) 0
      (add (add n (dec m)) (mult (dec m) (dec n))))) 
;Problem 5
(define (power b n)
  (if (= n 0) 1
          (mult (power b (dec n)) b)))
;Problem 6
(define (raise x n)
  (if  (= n 0) 1
       (if (= (modulo n 2) 0)
       (mult (raise x (/ n 2)) (raise x (/ n 2)))
       (mult (raise x (floor (/ n 2))) (mult (raise x (floor (/ n 2))) x)))))
;Problem 7
(define (pmult x y)
  (if (or (= x 0) (= y 0)) 0 
      (if (= (modulo x 2) 0) (pmult (/ x 2) (* y 2))
          (+ y (pmult (floor (/ x 2)) (* y 2)) ))))
;Problem 8
(define (sumEven n)
  (if (= n 0) 0
      (if (= (modulo n 2) 0)
          (+ n (sumEven (- n 2)))
          (if (not (= (modulo n 2) 0)) (sumEven (- n (modulo n 2)))
              ))))
(define (sumOdd n)
  (if (= n 1) 1
      (if (= n 0) 0
      (if (not (= (modulo n 2) 0))
          (+ n (sumOdd (- n 2)))
          (if (= (modulo n 2) 0)
              (sumOdd (- n 1)))))))
;Problem 9
(define (h-product k)
  (if (= k 1) k
      (* (- 1 (/ 1 k)) (h-product (- k 1)))))

;Problem 10
(define (divides a b) (= 0 (modulo b a)))
(define (divisors-upto n k)
  (if (= k 0) 0
      (if (= n 0) 0
          (if (= k 1) 1
              (if (divides k n)
                  (+ 1 (divisors-upto n (- k 1)))
                  (divisors-upto n (- k 1)))))))
(define (divisors n) (divisors-upto n n))
;Problem 11
(define (subfact n)
  (if (= n 0) 1
      (if (= n 1) 0
          (if (> n 1)
              (* (- n 1) (+ (subfact(- n 1)) (subfact(- n 2))))))))
;Problem 12
(define (factorial n)
  (if (= n 0) 1
      (* (factorial (- n 1)) n)))
(define (new-cos x n)  
      (if (= n 0) 1
      (+ (/ (* (expt -1 n) (expt x (* 2 n))) (factorial (* 2 n))) (new-cos x (- n 1)))))




  


            
      
      




  