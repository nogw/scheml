(define sum (lambda (lst)
	(if (null? lst)
		0
		(+ (car lst) (sum (cdr lst))))))

(define map (lambda (f lst)
	(if (null? lst)
		  '()
		  (cons (f (car lst)) (map f (cdr lst))))))

(define range (lambda (m n)
		(if (< n m)
		    '()
		    (cons m (range (+ m 1) n)))))

(define filter (lambda (p lst)
		 (if (null? lst)
		     '()
		     (if (p (car lst))
			 (cons (car lst) (filter p (cdr lst)))
			 (filter p (cdr lst))))))

(define fak (lambda (n)
	      (if (= n 0)
		  1
		  (* n (fak (- n 1))))))
