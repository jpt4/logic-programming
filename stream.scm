(load "minikanren.scm")
(load "mk.scm")

(define walk
  (lambda (i ls)
    (cond
      [(assoc i ls) (walk (cdr (assoc i ls)) ls)]
      [else i])))

(define check-walk
  (lambda (i ls)
    (cond
      [(assoc i ls) (walk i ls)]
      [else 'fail])))

;(define fib
 ; (lambda (n)
  ;  (cond
   ;   [(zero? n) => (lambda () 0) 'a])))

(define assoc-o
  (lambda (x l y)
    (conde
      [(fresh (a b c d)
         (conso a b l)
         (== `(,x . ,d) a)
         (== y a))]
      [(fresh (d)
         (cdro l d)
         (assoc-o x d y))])))

(define acc-assoc-o
  (lambda (i l o)
    (conde
      [(nullo l) (== i o)]
      [(fresh (a b c d)
         (conso a d l)
         (== `(,i . ,b) a)
         (== `(,a . ,c) o)
         (acc-assoc-o i d c))])))
;       [(fresh (d)
 ;         (cdro l d)
  ;        (acc-assoc-o i d o))])))

(define walk-o
  (lambda (i ls o)
    (conde
      [(fresh (a)
         (assoc-o i ls a)
         (nullo a)
         (== i o))]
      [(fresh (a b c d)
         (assoc-o i ls a)
         (cdro a d)
         (walk-o d ls b)
         (conso a b o))])))

(define remove-o
  (lambda (i l o)
    (conde
      [(nullo l) (== l o)]
      [(fresh (a b c d)
         (== `(,a . ,d) l)
         (== i a)
         (remove-o i d o))]
      [(fresh (a b c d)
         (== `(,a . ,d) i)
         (=/= i a)
         (== `(,a . ,o) b)
         (remove-o i d b))])))

(define reverse-o
  (lambda (i o)
    (reverse-o-aux i '() o)))

(define reverse-o-aux
  (lambda (i acc o)
    (conde
      [(nullo i) (== acc o)]
      [(fresh (a d newacc)
         (conso a d i)
         (conso a acc newacc)
         (reverse-o-aux d newacc o))])))


(define rev-o
  (lambda (i o)
    (conde
      [(nullo i) (nullo o)]
      [(singleton-o i) (== i o)]
      [(fresh (a b c d)
         (conso a d i)
         (conso b a o)
         (rev-o d b))])))
      
(define singleton-o
  (lambda (i)
    (fresh (a b c d)
      (conso a d i)
      (nullo d))))

(define fib-o
  (lambda (n o)
    (conde
      [(== '() n) (== '() o)]
      [(== '(1) n) (== '(1) o)]
      [(fresh (dec1 dec2 res1 res2)
         (=/= '() n)
         (=/= '(1) n)
         (minuso n '(1) dec1)
         (minuso n '(0 1) dec2)
         (pluso res1 res2 o)
         (fib-o dec1 res1)
         (fib-o dec2 res2))])))

(define gtheq-o
  (lambda (m n)
    (conde
      [(fresh (a)
         (pluso n a m))
         succeed])))

(define longer?-o
  (lambda (x y)
    (conde
      [(pairo x) (nullo y) succeed]
      [(pairo x) (pairo y)
       (fresh (xa xd ya yd)
         (conso xa xd x)
         (conso ya yd y)
         (longer?-o xd yd))])))

(define longer-o
  (lambda (x y o)
    (conde
      [(nullo x) (pairo y) (== y o)]
      [(pairo x) (nullo y) (== x o)]
      [(fresh (xa xd ya yd)
         (conso xa xd x)
         (conso ya yd y)
         (longer-o xd yd o))])))
