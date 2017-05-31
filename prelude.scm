;;  prelude.scm  jpt4  UTC20150630
;;  basic mk idioms

(define (add1o i o)
  (pluso i '(1) o))

(define (appendo ls e o)
  (conde
   [(null?o ls) (== e o)]
   [(list?o ls) (null?o e) (== ls o)]
   [(list?o ls) (=/= '() ls) (=/= '() e)
    (fresh (a)
     (== `(,a) ls)
     (conso a e o))]))

(define (caro i o)
  (fresh (ls)
    (== `(,o . ,ls) i)))

(define (cdro i o)
  (fresh (ls)
    (== `(,ls . ,o) i)))

(define (conso a d o)
  (== `(,a . ,d) o))

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

#|
(define (lengtho ls len)
  (length-acco ls '() len))

;;  accumulator style (lengtho)
(define (length-acco ls acc len)
  (conde
   [(null?o ls) (null?o acc) (null?o len)]
   [(fresh (ls-val)
           (null?o acc) 
           (non-null?o ls) (== ls ls-val)
           (laccauxo ls acc len))]
   [(non-null?o acc) 'diag (== len 'err-non-null-acc-initial-value)
    ]))
(define (laccauxo ls acc len)
  (conde
   [(null?o ls) (non-null?o acc) (== acc len)]
   [(fresh (lsa lsd res)
      (add1o acc res)
      (== `(,lsa . ,lsd) ls)
      (laccauxo lsd res len))]))
|#

(define lengtho
(lambda (ls num)
(conde
     [(null?o ls) (== num '())]
     [(fresh (a d res)
             (== `(,a . ,d) ls)
             (lengtho d res)
             (pluso '(1) res num)
             )])))

(define (listo a d o)
  (== `(,a ,d) o))

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
