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

(define (flatteno i o)      ;;  TODO: Need to fix conde overlap
  (conde
    [(null?o i) (null?o o)]
    [(fresh (a d aa aa2 ad res)
       (== `(,a . ,d) i)    ;;  i is a list
       (=/= `(,aa . ()) a)  ;;  (car i) is not a list
       (== `(,aa2 . ,ad) a)
       (== aa2 aa)
       (== `(,a . ,res) o)
       (flatteno d res))]
    [(fresh (a aa d res)
       (== `(,a . ,d) i)
       (== `(,aa . ()) a)   ;;  (car a) is a list
       (== `(,a . ,res) o)
       (flatteno d res))]))

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
