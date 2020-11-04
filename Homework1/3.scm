;БЕЛЕЖКИ
;(string #\a) --> "a"
;(string-length "jsdf") --> 4
;(string-ref "abc" 1) --> b
;(string-append "abc" "def") --> "abcdef"
;(substring "abc" 1 3)
;(make-string 3 z) --> "zzz"
;(char->integer
;(char-whitespace? ["\n", "\t") --> #t else #f
;оператори --> +, -, /, *, ^, %,
;: Взето от Доцент,Доктор Атанас Семерджиев --> Дискорд

(define plus "+")
(define minus "-")
(define mult "*")
(define div "/")
(define power "^")

(define (char-digit? c) (and (char>=? c #\0) (char<=? c #\9)))
(define (char-operator? c) (or (eq? plus c) (eq? minus c) (eq? mult c) (eq? div c) (eq? power c)))

(define (1- x) (- x 1)) 
(define (accumulate-opp op term init a next b)
  (define (loop i)
      (if (>= i b)
          (op (term i) (loop (next i)) )
          init
  ))
  (loop a)
)

(define (remove-whitespaces str)
  (define len (string-length str))
  (define (helper k i res)
    (cond  ((<= i k) res)
           ((char-whitespace? (string-ref str i)) (helper k (- i 1) res))
           (else (helper k (- i 1) (string-append (string(string-ref str i)) res)))))
  (helper 0 (- len 1) ""))

(define (expr-valid? str)
  (define newstr (remove-whitespaces str))
  

  
