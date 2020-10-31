;; up-left-corner -->    \u250C
;; horizontal -->        \u2500
;; vertical     -->      \u2502
;; up-right-corner -->   \u2510
;; down-left-corner -->  \u2514
;; down-right-corner --> \u2518

;; Accumulate:

(define (accumulate op term init a next b)  
  (define (loop i)
      (if (<= i b)
          (op (term i) (loop (next i)) )
          init
  ))
  (loop a)
)

(define (how-many-squares-horziontals n)
  (cond ((= n 1 ) 1)
        (else (+ 4 (how-many-squares-horziontals (- n 1))))))

(define (how-many-squares-verticals n)
  (cond ((= n 1) 0)
        (else (+ 2 (how-many-squares-verticals (- n 1))))))

