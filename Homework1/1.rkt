;; up-left-corner -->    \u250C
;; horizontal -->        \u2500
;; vertical     -->      \u2502
;; up-right-corner -->   \u2510
;; down-left-corner -->  \u2514
;; down-right-corner --> \u2518

;; Понеже хоризонталните линии са повече от вертикалните
;; и от пример се вижда, че при 3 квадрата има 9 хоризонтални
;; ------------- // ----------- 2 квадрата има 5 и т.н
;; (тоест или се увеличават с 4, или се намалят с 4 в заисимост от това колко са квадратите
(define (how-many-squares-horziontals n)
  (cond ((= n 1 ) 1)
        (else (+ 4 (how-many-squares-horziontals (- n 1))))))

;; обяснението е същото, което следва от примера даден в Домашшно 1
(define (how-many-squares-verticals n)
  (cond ( (= n 1) 0)
        (else (+ 2 (how-many-squares-verticals (- n 1))))))
;; рисува горната линия + двата ъгъла
(define (squares-draw-up-line n)
  (display "\u250C")
  (display (make-string (how-many-squares-horziontals n) #\u2500))
  (display "\u2510"))
;; рисува долната линия + двата ъгъла
(define (squares-draw-down-line n)
  (display "\u2514")
  (display (make-string (how-many-squares-horziontals n) #\u2500))
  (display "\u2518"))









