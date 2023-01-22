;problem 1

(define (list-at l i)
  (if (= i 0) (car l)
      (list-at (cdr l) (- i 1))))

;1b
(define (smallest l)
  (define (smaller a b)
    (if (< a b) a
        b))
  (if (null? (cdr l)) ; '(1)
      (car l) ; 1
      (smaller (car l) (smallest (cdr l))))) ; (2 3 4 1)
(define (remove v elements)
  (if (null? elements) elements
      (if (equal? v (car elements)) ; (1 2 3) => (2 3)
          (cdr elements)
          (cons (car elements) (remove v (cdr elements))))))
(define (selSort l)
  (if (null? l)
      '()
      (let* ((first (smallest l)) ; (3 2 1) => 1
             (rest (remove first l))); (3 2)
        (cons first (selSort rest)))))


(define (list-median l)
  (define (length l)
    (if (null? l) 0
        (+ 1 (length (cdr l)))))
  (let ((sort (selSort l)))
  (if (= (modulo (length l) 2) 0)
      (/ (+ (list-at sort (floor (/ (length l) 2) )) (list-at sort (- (floor (/ (length l) 2)) 1))) 2)
      (list-at sort (floor (/ (length l) 2))))))

;problem 2

(define (explode x)
  (if (< x 10) (list x)
      (append (explode (floor (/ x 10)))
      (list (- x (* 10 (floor (/ x 10)) )) ))))

(define (implode x)
  (define (digits x i)
    (if (null? x) 0
        (+ (* (car x) (expt 10 i))
           (digits (cdr x) (+ i 1)))))
  (digits (reverse x) 0))
    
(define (has-property x)
  (define (sum-list l)
    (if (null? l) 0
        (+ (car l) (sum-list (cdr l)))))

  (let* ((num (explode x))
         (sum-num (sum-list num))
         (sum (explode sum-num))
         (sum-final (implode (reverse sum))))
    (= (* sum-final sum-num) x)))

(define (find sequence test n)
  (define (help x i)
        (if (test (sequence x))
            (if (= (+ i 1) n) 
                (sequence x)
            (help (+ x 1) (+ i 1)))
            (help (+ x 1) i)))
  (help 1 0))

(define (fujiwara n)
  (find (lambda (x) x) has-property n))
            

  



