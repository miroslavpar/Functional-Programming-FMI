;;Помощни функции:
(define (get-last-digit n) (remainder n 10))

(define (del-num-by-ten n) (quotient n 10))


(define (toBinary n)
  (define (helper n res bit)
    (if (= n 0) res
        (helper (quotient n 2)
                (+ res (* (remainder n 2) (expt 10 bit)))
                (+ bit 1))))
  (helper n 0 0))


(define (bits? n)
  (define (helper n count)
    (cond ((= n 0) count)
          (else (helper (del-num-by-ten n) (+ 1 count)))))
  (helper (toBinary n) 0))


(define (toDecimal n)
  (if (= n 0) 0
      (+ (get-last-digit n) (* 2 (toDecimal (quotient n 10))))))

(define (generic oper1 oper2 s1 s2)
  (define (helper newSet1 newSet2 res bit)
    (cond ((oper1 (= newSet1 0) (= newSet2 0)) (toDecimal res))
          ((oper2 (= 1 (get-last-digit newSet1)) (= 1 (get-last-digit newSet2))) (helper (del-num-by-ten newSet1) (del-num-by-ten newSet2) (+ res (* 1 (expt 10 bit))) (+ bit 1)))
          (else (helper (del-num-by-ten newSet1) (del-num-by-ten newSet2) (+ res (* 0 (expt 10 bit))) (+ bit 1)))))
  (helper (toBinary s1) (toBinary s2) 0 0))

;;Основни функции

(define (contains? n elem)
  (define (helper n bits)
    (cond ((= n 0) #f)
          ((and (= bits elem) (= 1 (remainder n 2)))  #t)
          (else (helper (del-num-by-ten n) (+ 1 bits)))))
  (helper (toBinary n) 0))

(define (set-add set elem)
  (if (or (contains? set elem) (negative? elem)) set
      (+ set (expt 2 elem))))

(define (set-remove set elem)
  (if (or (not (contains? set elem)) (negative? elem))set
      (- set (expt 2 elem))))

(define (set-empty? set)
  (if (= 0 set)#t #f))

(define (set-size set)
  (define (helper newset count)
    (cond ((= newset 0) count)
          ((= 1 (remainder newset 10)) (helper (del-num-by-ten newset) (+ count 1)))
          (else (helper (del-num-by-ten newset) count))))
  (helper (toBinary set) 0))

(define (set-intersect s1 s2)
  (generic (lambda (a b) (or a b)) (lambda (a b) (and a b)) s1 s2))


(define (set-union s1 s2)
  (generic (lambda (a b) (and a b)) (lambda (a b) (or a b)) s1 s2))


(define (set-differnece s1 s2)
  (generic (lambda (a b) (and a b)) (lambda (a b) (and a b)) s1 s2))
       