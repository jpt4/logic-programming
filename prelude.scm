;;  prelude.scm  jpt4  UTC20150630
;;  basic mk idioms

(define (add1o i o)
  (pluso i '(1) o))

(define (caro i o)
  (fresh (ls)
    (== `(o . ls) i)))

(define (cdro i o)
  (fresh (ls)
    (== `(ls . o) i)))

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
       (== `(,aa . ,resa) o)
       (flatten*o aa resa)
;       (flatten*o d resd)   
 ;      (== `(,resa . ,resd) o)
     )]))

       

(define (lengtho ls len o)
  (conde
    [(null?o ls) (== len o)]
    [(fresh (lsa lsd res)
       (== `(,lsa . ,lsd) ls)
       (add1o len res)
       (lengtho lsd res o))]))

(define (non-null?o i)
  (=/= '() i))

(define (null?o i)
  (== '() i))

(define (sub1o i o)
  (add1o o i))
