(define range (lambda (m n) 
  (if (< n m) 
    '() 
    (cons m (range (+ m 1) n)))))

(define sum (lambda (a) (+ a 1)))

(display (range 1 10))