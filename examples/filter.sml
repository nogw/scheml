(define even? (lambda (v) 
  (= (modulo v 2) 0)))

(define range (lambda (m n) 
  (if (< n m) 
    '() 
    (cons m (range (+ m 1) n)))))

(define filter (lambda (fil lst)
  (if (null? lst)
    '()
    (if (fil (car lst))
      (cons (car lst) (filter fil (cdr lst)))
      (filter fil (cdr lst))))))

(define result (filter even? (range 1 10)))