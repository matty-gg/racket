;Problem 1
(define (make-tree value left right)
(list value left right))
(define (value T) (car T))
(define (right T) (caddr T))
(define (left T) (cadr T))

(define (tree-size T)
  (if (null? T) 0
      (+ 1 (tree-size (left T)) (tree-size (right T)))))
;Problem 2
(define (tree-depth T)
  (if (null? T) -1
      (+ 1 (max (tree-depth (left T)) (tree-depth (right T))))))
;Problem 3
(define (count-pred P tree)
  (if (null? tree) 0
      (if (equal? (P (value tree)) #t)
          (+ 1 (count-pred P (left tree))
          (count-pred P (right tree)))
      (+ 0 (count-pred P (left tree)) (count-pred P (right tree))))))
;Problem 4
(define (count-one-child T)
  (let* ((left-t (left T))
         (right-t (right T)))
    (cond ((and (null? left-t) (null? right-t)) 0)
          ((and (not (null? left-t)) (not (null? right-t)))
           (+ (count-one-child (left T)) (count-one-child (right T))))
          ((null? left-t)
           (+ 1 (count-one-child right-t)))
           (else (+ 1 (count-one-child left-t))))
))
;Problem 5
(define (invert-bst T)
  (if (null? T) T
      (make-tree (value T) (invert-bst (right T)) (invert-bst (left T)))))
(invert-bst (list 33 (list 22 (list 11 (list) (list)) (list)) (list)))