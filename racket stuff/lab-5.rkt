;1
(define (square-pair x)
  (cons x (* x x)))
;2
(define (rev p)
  (cons (cdr p) (car p)))
;3a
(define (c->p p)
  (let ((r (sqrt(+ (* (car p) (car p)) (* (cdr p) (cdr p))))))
    (cons r (atan (/ (cdr p) (car p))))))
;3b
(define (p->c p)
  (let ((x (* (car p) (cos (cdr p))))
        (y (* (car p) (sin (cdr p)))))
    (cons x y)))
;4
(define (y p1 p2)
  (let* ((m (/ (- (cdr p2) (cdr p1)) (- (car p2) (car p1))))
         (b (- (cdr p1) (* m (car p1)))))
    (lambda (x) (+ b (* m x)))))
