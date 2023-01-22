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
(define (intSort l)
  
