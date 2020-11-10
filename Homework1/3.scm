;;;;;;;;;;;;;;;;;;;;Константи;;;;;;;;;;;;;;;;;;;;
(define plus #\+)
(define minus #\-)
(define mult #\*)
(define div #\/)
(define power #\^)
(define separator ",")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (char-digit? c) (and (char>=? c #\0) (char<=? c #\9)))

(define (char-operator? c) (or (eq? plus c) (eq? minus c) (eq? mult c) (eq? div c) (eq? power c)))
;;;;;;;;;;;;;;;;;;;;;;;;(Недовършено б) и в));;;;;;;;;;;;;;;;;;;;;;;;
;Stack фунцкии :

(define (isempty? expr)
  (if (= (string-length expr) 0)#t #f))

(define (top expr)
  (define len (string-length expr))
  (define (helper i res)
    (if (or (<= i 0) (string=? (string(string-ref expr i)) separator))
        res
        (helper (- i 1) (string-append (string (string-ref expr i)) res))))
  (if (= (string-length expr) 0)#f (helper (- len 1) "")))

; стек(импровизиран) от числа
 
(define (nums-in-str-in-stack str)
  (define len (string-length str))
  (define (helper i res)
    (cond  ((< i 0) res)
           ((char-digit? (string-ref str i)) (helper (- i 1) (string-append (string(string-ref str i)) res)))
           (else (helper (- i 1) (string-append separator res)))))
  (helper (- len 1) ""))
; стек(импровизиран) от опеартори
(define (op-in-str-in-stack str)
  (define len (string-length str))
  (define (helper i res)
    (cond  ((< i 0) res)
           ((char-operator? (string-ref str i)) (helper (- i 1) (string-append (string(string-ref str i)) res)))
           (else (if (char-digit? (string-ref str i))(helper (- i 1) res) (helper (- i 1) (string-append separator res)))))
  )(helper (- len 1) ""))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;премахване на спейсове 
(define (remove-whitespaces str)
  (define len (string-length str))
  (define (helper i res)
    (cond  ((< i 0) res)
           ((char-whitespace? (string-ref str i)) (helper (- i 1) res))
           (else (helper (- i 1) (string-append (string(string-ref str i)) res)))))
  (helper (- len 1) ""))

;;generic функция за индексите
(define (generic op str ind)
  (define len (string-length str))
  (cond ((= len (+ 1 ind)) (+ ind 1))
        ((op(string-ref str (+ ind 1))) (generic op str (+ ind 1)))
        (else (+ ind 1))))
;;Като види число ми връща индекс след числото а.к.а прескача числото и отива към индекса след него
(define (numberafter str index)
  (generic char-digit? str index))

;; --------------//------------------------ 
(define (whitespaceafter str index)
  (generic char-whitespace? str index))

;;Ако някъде делим на 0 в израза
(define (delbyzero? str)
  (define len (string-length str))
  (define (helper i len  str return)
    (cond ((= i len)#f)
          ((and
            (eq? (string-ref str i) #\/)
            (if (char-whitespace?(string-ref str i))(eq? #\0 (string-ref str (whitespaceafter str i))))) (or return #f))
          (else (helper (+ i 1) len str #t))))
  (helper 0 len str #t))

;;последно видяно число --->  1
;;последно видян опеаратор ---> 0
(define (one? x) (if (= x 1)#t #f))
(define (isempty? x) (equal? x ""))

(define (expr-valid? str)
  (define len (string-length str))
  (define (helper str i lastseen)
    (cond ((delbyzero? str)#f)
          ((isempty? str)#t)
          ((and (>= i len) (zero? lastseen)) #f)
          ((and (>= i len) (one? lastseen)) #t)
          (else (if (zero? lastseen)
                    (cond 
                  ((char-digit? (string-ref str i)) (helper str (numberafter str i) 1))
                  ((char-whitespace?(string-ref str i )) (helper str (whitespaceafter str i) lastseen))
                  (else #f))
                    (cond
                      ((char-digit? (string-ref str i)) #f)
                      ((char-whitespace? (string-ref str i)) (helper str (whitespaceafter str i) lastseen))
                      (else (helper str (+ i 1) 0)))))))
  (helper str 0 0))