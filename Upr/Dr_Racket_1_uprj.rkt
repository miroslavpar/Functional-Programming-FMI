;Задача 1
(define (interval-int a b)
  (define (helper x res)
    (cond ((> x b) res)
          (else (helper (+ x 1) (+ x res)))))
  
  (if (<= a b) (helper a 0))
)
(interval-int 2 4)

;Задача 2
(define (sq x) (* x x))
(define (fast-exponential t n)
  (cond ((= n 0) 1)
        ((= 0 (remainder n 2)) (sq (fast-exponential t (quotient n 2 ))))
        (else (* t (sq (fast-exponential t (quotient n 2 )))))))
(fast-exponential 5 3)

;Задача 3
(define (get-last-digit n) (remainder n 10))
(define (count-digit n d)
  (define (helper newN count)
    (cond ((= newN 0) count)
          ((= (get-last-digit newN) d) (helper(quotient newN 10) (+ count 1)))
          (else helper (quotient newN 10) count)))
  (if (= n 0)
      (if(= d 0) 1  0)
      (helper n 0))
  )
  (count-digit 0 0)


;Задача 4
(define (reverse-int n)
  (define (helper newN lastD)
    (cond ((= 0 newN) lastD)
          (else (helper (quotient newN 10) (+ (* 10 lastD) (get-last-digit newN))))))
  (helper n 0)
  )

(reverse-int 123)

;Задачча 5
(define (palindrome? n ) (= n (reverse-int n)))

(palindrome? 123321)

;Задача 6
(define (divisors-sum n)
  (define (helper div res)
    (cond ((> div n) res)
           ((= (remainder n div) 0) (helper (+ 1 div) (+ res div)))
           (else (helper (+ div 1) res))))
  (helper 1 0)
  )

(divisors-sum 12)

;Задача 7
(define (perfect? n) (= n (- (divisors-sum n ) n)))

(perfect? 6)

;Задача 8
;Моя имплементация за sqrt (не знаех, че вече съществува)
(define (sqrtT n)
  (define (helper div)
    (cond ((= n (sq div)) div)
           (else (helper (+ 1 div)))))
  (helper 1))
(sqrtT 9)
  
;(define (prime? n)
  ;(if (or (= n 0) (= n 1)) #f)
   ;(define (helper div)
    ; (if (= 0 (remainder (sqrtT n) d)) #f)
     ;           (else (helper (+ div 1))))
  ;  (helper 2))
;(prime? 101)

;; УПРАЖНЕНИЕ 20.10.2020г.

;Задача 1
(define (toBinary n)
  (if (= n 0 ) 0
      (+ (remainder n 2) (* 10 (toBinary (quotient n 2))))))





          
              


        