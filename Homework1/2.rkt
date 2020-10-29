;;Помощни функции:

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
          (else (helper (quotient n 10) (+ 1 count)))))
  (helper (toBinary n) 0))


(define (toDecimal n)
  (if (= n 0) 0
      (+ (remainder n 10) (* 2 (toDecimal (quotient n 10))))))
;;Основни функции

(define (contains? n elem)
  (define (helper n bits)
    (cond ((= n 0) #f)
          ((and (= bits elem) (= 1 (remainder n 2)))  #t)
          (else (helper (quotient n 10) (+ 1 bits)))))
  (helper (toBinary n) 0))


(define (set-add set elem)
  (define (helper newSet res bit)
    (cond ((contains? (toBinary set) elem) (toBinary set))
          ((= 0 newSet) (+ res (* 1 (expt 10 elem))))
          ((= elem bit) res)
          (else (helper (quotient newSet 2) (+ res (* (remainder newSet 2) (expt 10 bit))) (+ bit 1)))))
    (toDecimal (helper set 0 0)))

(define (set-empty? set)
  (if (= 0 set)#t #f))


(define (set-size set)
  (define (helper newset count)
    (cond ((= newset 0) count)
          ((= 1 (remainder newset 10)) (helper (quotient newset 10) (+ count 1)))
          (else (helper (quotient newset 10) count))))
  (helper (toBinary set) 0))