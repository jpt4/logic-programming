;;  prelude.scm  jpt4  UTC20150630
;;  basic mk idioms

(define (null?o i)
  (== '() i))

(define (caro i o)
  (fresh (ls)
    (== `(o . ls) i)))

(define (cdro i o)
  (fresh (ls)
    (== `(ls . o) i)))

(define (add1o i o)
  (pluso i '(1) o))

(define (sub1o i o)
  (add1o o i))

(define (lengtho ls len o)
  (conde
    [(null?o ls) (== len o)]
    [(fresh (lsa lsd res)
       (== `(,lsa . ,lsd) ls)
       (add1o len res)
       (lengtho lsd res o))]))
       
