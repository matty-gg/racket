;SORTING FUNCTION
(define (selSort l)
  (define (smallest l)
  (define (smaller a b) (if (< a b) a b))
  (if (null? (cdr l)) 
      (car l)
      (smaller (car l) (smallest (cdr l)))))
  (define (remove v elements)
  (if (null? elements) elements
      (if (equal? v (car elements)) 
          (cdr elements)
          (cons (car elements) (remove v (cdr elements))))))
  (if (null? l)
      '()
      (let* ((first (smallest l)) 
             (rest (remove first l)))
        (cons first (selSort rest)))))
;HELPER FUNCTIONS
    (define (remove v elements)
  (if (null? elements) elements
      (if (equal? v (car elements)) 
          (cdr elements)
          (cons (car elements) (remove v (cdr elements))))))

  (define (prime? n)
  (define (divisor? k) (= 0 (modulo n k)))
  (define (divisors-upto k)
    (and (> k 1)
         (or (divisor? k) (divisors-upto (- k 1)))))
  (not (divisors-upto (- n 1))))

;Problem 1
(define (merge l1 l2)
  (if (or (null? l1)
      (null? l2))
      '()
      (selSort (append (merge (cdr l1) (cdr l2)) (list (car l1) (car l2))))))
;Problem 2
(define (mergeSort l)
  (define (smallest l)
  (define (small a b) (if (< a b) a b))
  (if (null? (cdr l)) (car l)
      (small (car l) (smallest (cdr l)))))
  (define (remove v elements)
  (if (null? elements) elements
      (if (equal? v (car elements)) 
          (cdr elements)
          (cons (car elements) (remove v (cdr elements))))))
  (if (null? l) '()
      (let* ((1st (smallest l))
             (rest (remove 1st l)))
  (cons 1st (mergeSort rest)))))
;Problem 3
(define (ins x l)
  (mergeSort (cons x l)))
;Problem 4
(define (insSort l)
    (define (smallest l)
  (define (small a b) (if (< a b) a b))
  (if (null? l) '()
      (if (null? (cdr l)) (car l)
      (small (car l) (smallest (cdr l))))))
  (cdr (ins (smallest l) l)))

;Problem 5
;5a
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) 
  (fold-right op initial (cdr sequence)))))
;5b
(define (fold-left op initial sequence)
  (if (null? sequence)
      initial
         (fold-left op (op initial (car sequence)) (cdr sequence)) ))
;5c
(define (my-map p sequence)
(fold-right (lambda (x y) (cons (p x) y)) '() sequence))
;5d
(define (my-append seq1 seq2) (fold-right cons seq2 seq1))
;5e
(define (my-length sequence) (fold-right (lambda (x y) (+ 1 y)) 0 sequence))
;5f
(define (reverse-r sequence)
  (fold-right (lambda (x y) (my-append y (list x))) '() sequence))
;5g
(define (reverse-l sequence)
(fold-left (lambda (x y) (cons y x)) '() sequence))
;5h
(define (horner-eval x coefficient-list)
  (fold-right (lambda (w y) (+ (* y x) w)) 0 coefficient-list))
;Problem 6
;6ai
(define (count n x)
  (if (< n 10) (+ x 1)
      (count (/ n 10) (+ x 1))))
(count 345 0)
(define (left-truncatable-prime? p)
  (if (= p 1) #f
      (if (prime? p)
          (cond ((< p 10) #t)
                (else (left-truncatable-prime? (modulo p (expt 10 (- (count p 0) 1) ) )))
          ) #f))
  )
;6aii
(define (find sequence test n)
  (define (help x i)
        (if (test (sequence x))
            (if (= (+ i 1) n) 
                (sequence x)
            (help (+ x 1) (+ i 1)))
            (help (+ x 1) i)))
  (help 1 0))
(define (nth-left-trunc-prime n)
  (find (lambda (x) (floor (/ x 10))) left-truncatable-prime?  n))
(nth-left-trunc-prime 29)
;6bi
;(define (right-truncatable-prime? p)
 