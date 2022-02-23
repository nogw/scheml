(define sum (lambda (a)
  (+ a 1)))

(define range (lambda (m n) 
  (if (< n m) 
    '() 
    (cons m (range (+ m 1) n)))))

(define map (lambda (f lst) 
  (if (null? lst) 
    '() 
    (cons (f (car lst)) (map f (cdr lst))))))

(define result (map sum (range 1 10)))