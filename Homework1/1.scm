;; up-left-corner -->    \u250C
;; horizontal -->        \u2500
;; vertical     -->      \u2502
;; up-right-corner -->   \u2510
;; down-left-corner -->  \u2514
;; down-right-corner --> \u2518
;; space ---> \u0020

;;БЕЛЕЖКИ:

;;string-append -----> "\u0020" - тази фунцкяи приема стринга така
;;make-string ------> #\u2502 - тази така 

;; при n = 3 -> 9 хоризонтални и така през 4 в зависимост от n --> формула (4 * n) -3
;; при n = 3 -> 4 вертиканли и така през 2 в зависимост от n ----> формула (


;; Accumulate:

(define (accumulate op term init a next b)  
  (define (loop i)
      (if (<= i b)
          (op (term i) (loop (next i)))
          init
  ))
  (loop a)
)
(define (accumulate-opp op term init a next b)  
  (define (loop i)
      (if (>= i b)
          (op (term i) (loop (next i)) )
          init
  ))
  (loop a)
)

(define (multiple-string num sym1 sym2)
  (define (helper k res)
    (cond ((= k 0) res)
          (else (helper (- k 1) (string-append res (string-append sym1 sym2))))))
  (helper num ""))

(define (draw-boxes n _)
  (define (helper k)
  (if (> k 0)
      (begin
                (display (multiple-string (- n k) "\u2502" "\u0020"))
                (display "\u250C")
                (display (make-string (- (* 4 k) 3) #\u2500))
                (display "\u2510")
                (display (multiple-string (- n k) "\u0020" "\u2502"))
                (display "\n")
                (helper (- k 1))
                (display (multiple-string (- n k) "\u2502" "\u0020"))
                (display "\u2514")
                (display (make-string (- (* 4 k) 3) #\u2500))
                (display "\u2518")
                (display (multiple-string (- n k) "\u0020" "\u2502"))
                (display "\n"))
      ))
 (helper n))

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))
(define (id x) x)
(define (squares n)
  (accumulate draw-boxes id 0 n 1+ n))