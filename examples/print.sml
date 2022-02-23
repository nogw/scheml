(define range (lambda (m n) 
  (if (< n m) 
    '() 
    (cons m (range (+ m 1) n)))))

(define iter (lambda (f lst)
  (if (null? lst)
    '()
    (cons (f (car lst)) (iter f (cdr lst)))))) 

(display (range 1 10))