(define (take n lst)
 (if (or (= n 0) (null? lst)) '()
     (cons (car lst) (take (- n 1) (cdr lst)))))
(take 3 '(1 2 3 4 5 6 7 8) )

(define (drop n lst)
  (if (or (= n 0) (null? lst)) lst
      (drop (- n 1) (cdr lst))))
(drop 3 '(1 2 3 4 5) )

(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2)) '()
      (cons (car lst1) (car lst2)
             (zip (cdr lst1) (cdr lst2)))))

(zip '(1 2 3 4) '(5 6 7 8))

(define (sorted? lst)
  (if (or (null? lst) (null? (cdr lst)) #t)
      (sorted