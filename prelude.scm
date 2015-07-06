;;  prelude.scm  jpt4  UTC20150630
;;  basic mk idioms

(define (add1o i o)
  (pluso i '(1) o))

(define (caro i o)
  (fresh (ls)
    (== `(,o . ,ls) i)))

(define (cdro i o)
  (fresh (ls)
    (== `(,ls . ,o) i)))

;;  flat?o - list without recursive elements
(define (flat?o i)
  (conde
    [(null?o i)]
    [(fresh (a d)
       (caro i a) (cdro i d) (singleton?o a) (flat?o d))]))

(define (singleton?o i)
  (conde
    [(symbolo i)]
    [(numbero i)]))
    
;;  (flatteno) output is not uniquely identifying of input. To create 
;;  bidirectionality, a degree of flattening must be specified. (Compare TFP's
;;  recursion depth.

(define (flatteno i o)       ;;  But it must work for one deep.
  (conde
    [(null?o i) (null?o o)]
    [(fresh (a aa d res)     ;;  singleton (car i)
       (== `(,a . ,d) i)    
       (=/= `(,aa . ()) a)  
       (== `(,a . ,res) o)
       (flatteno d res))]
    [(fresh (a aa d res)     ;;  list (car i)
       (== `(,a . ,d) i)
       (== `(,aa . ()) a)   
       (== `(,aa . ,res) o)
       (flatteno d res))]))

(define (flatten*o i o)
  (conde
    [(null?o i) (null?o o)]
    [(fresh (a aa d res)     ;;  singleton (car i)
       (== `(,a . ,d) i)    
       (=/= `(,aa . ()) a)  
       (== `(,a . ,res) o)
       (flatten*o d res))]
    [(fresh (a aa d resa resd)     ;;  list (car i)
       (== `(,a . ,d) i)
       (== `(,aa . ()) a)
;       (== `(,aa . ,resa) o)
       (flatten*o aa o)
;       (flatten*o d resd)   
 ;      (== `(,resa . ,resd) o)
     )]))

;;  Test results
#|

> (run 1 (q) (flatten*o q '(1 2 3)))
((1 2 3))
> (run 2 (q) (flatten*o q '(1 2 3)))
((1 2 3) (1 2 3 (()) . _.0))
> (run 3 (q) (flatten*o q '(1 2 3)))
((1 2 3) (1 2 3 (()) . _.0) (((1 2 3)) . _.0))
> (run 10 (q) (flatten*o q '(1 2 3)))
((1 2 3) (1 2 3 (()) . _.0) (((1 2 3)) . _.0) (1 ((2 3)) . _.0)
  (1 2 ((3)) . _.0) (1 2 3 (((()) . _.0)) . _.1)
  (1 2 3 (((((()) . _.0)) . _.1)) . _.2)
  (((1 2 3 (()) . _.0)) . _.1) (1 ((2 3 (()) . _.0)) . _.1)
  (((((1 2 3)) . _.0)) . _.1))
> (run 10 (q) (flatten*o '((1) 2 3) '(1 2 3)))
()
>
|#

(define (lengtho ls o)
  (conde
    [(null?o ls) (== '() o)]
    [(fresh (a d res)
       (== `(,a . ,d) ls)
       (add1o res o) (lengtho d res))]))

;;  accumulator style (lengtho)
(define (length-acco ls len o)
  (conde
    [(null?o ls) (== len o)]
    [(fresh (lsa lsd res)
       (== `(,lsa . ,lsd) ls)
       (add1o len res)
       (length-acco lsd res o))]))

(define (list?o ls)
  (conde
    [(null?o ls)]
    [(fresh (a b)
       (== `(,a . ,b) ls))]))

(define (lol?o ls)
  (conde
    [(fresh (a d)
       (caro ls a) (cdro ls d) (list?o a) (null?o d))]
    [(fresh (a d)
       (caro ls a) (list?o a) (cdro ls d)
       (lol?o d))]))

(define (non-null?o i)
  (=/= '() i))

(define (null?o i)
  (== '() i))

(define (pair?o p)
  (fresh (a d)
    (== `(,a . ,d) p)))

(define (sub1o i o)
  (add1o o i))
