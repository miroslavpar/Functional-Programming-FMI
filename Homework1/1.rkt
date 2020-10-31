;; up-left-corner -->    \u250C
;; horizontal -->        \u2500
;; vertical     -->      \u2502
;; up-right-corner -->   \u2510
;; down-left-corner -->  \u2514
;; down-right-corner --> \u2518

;;БЕЛЕЖКИ:

;; при n = 3 -> 9 хоризонтални и така през 4 в зависимост от n
;; при n = 3 -> 4 вертиканли и така през 2 в зависимост от n


;; Accumulate:

(define (accumulate op term init a next b)  
  (define (loop i)
      (if (<= i b)
          (op (term i) (loop (next i)) )
          init
  ))
  (loop a)
)

(define (draw-up-line n fic)
  (define (helper k)
   (display (make-string (- n k) #\u0020))
   (display "\u250C")
   (display (make-string (- (* 4 k) 3) #\u2500))
   (display "\u2510")
   (display "\n"))
  (helper n))

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))
(define (id x) x)
(accumulate draw-up-line id 0 1 1+ 3)



















;(define (how-many-squares-horziontals n)9) ;--> тук ще ми е параметърът от ХЕЛП фунцкията,т.е accumulate тръгва от 1 и до subsquares 
;;  (cond ((= n 1) 1)
;;        (else (+ 4 (how-many-squares-horziontals (- n 1))))))
;
;(define (how-many-squares-verticals n)9) ;--> и тук също ---//----
;;  (cond ((= n 1) 0)
;;        (else (+ 2 (how-many-squares-verticals (- n 1))))))
;
;(define (draw-up-line fic n)
;  (define (helper k)
;   (display "\u250C")
;   (display (make-string (- (* 4 k) 3) #\u2500))
;   (display "\u2510")
;   (display "space"))
;  (helper n))
;
;(define (squares-draw-down-line n)
;  (define (helper k)
;   (display "\u2514")
;   (display (make-string (- (* 4 k) 3) #\u2500))
;   (display "\u2518"))
;  (helper n))
;
;(define (1- x) (- x 1))
;(define (id x) x )
;(accumulate draw-up-line id 0 3 1- 1)
;
;;(define (squares n))