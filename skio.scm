;;skio.scm
;;utc20170530
;;jpt4
;;Relational SKI combinator calculus interpreter
;;Chez Scheme v9.4-1

(load "miniKanren-with-symbolic-constraints/mk.scm")

;;the universe of variables
(define (varo i)
  (conde
   [(symbolo i)
    (=/= 'I i) (=/= 'K i) (=/= 'S i)]))
;;and combinators
(define (combo i)
  (conde
   [(== 'I i)]
   [(== 'K i)]
   [(== 'S i)]))
;;terms thereof
(define (termo i)
  (conde
   [(varo i)]
   [(combo i)]
   [(fresh (a d)
     (== `(,a ,d) i)
     (termo a) (termo d))]))

;;coerce left association in expressions
(define (laso i o)
  (conde
   [(termo i) (== i o)]
   [(fresh (a b resa resb)
     (== `(,a ,b) i) 
     (laso a resa) (laso b resb) (== `(,resa ,resb) o))]
   [(fresh (a b c resa resb resc)
     (== `(,a ,b ,c) i) 
     (laso a resa) (laso b resb) (laso c resc)
     (== `((,resa ,resb) ,resc) o))]
   [(fresh (a b c resa resb resc)
     (== `(,a ,b . ,c) i) (=/= '() c)
     (laso a resa) (laso b resb) (laso `((,resa ,resb) . ,c) o)
     )]))

;;combinator reductions
(define (io i o)
  (fresh (x)
   (conde
    [(== `(I ,x) i) (== x o)])))
(define (ko i o)
  (fresh (x y)
   (conde
    [(== `((K ,x) ,y) i) (== x o)])))
(define (so i o)
  (fresh (x y z)
   (conde
    [(== `(((S ,x) ,y) ,z) i)  (== `((,x ,z) (,y ,z)) o)])))

;;core interpreter
#;(define (skio-aux i h o)
  (fresh (a b c d resa resb resh res)
   (conde
    [(== `(,a ,a) h) (== i a) (== i o)]
    [(conde
      [(varo i) (== `(,a ,b) h) (== `(,i ,a) resh) (skio-aux i resh o)]
      [(combo i) (== `(,a ,b) h) (== `(,i ,a) resh) (skio-aux i resh o)]
      [(io i res) (== `(,a ,b) h) (== `(,i ,a) resh) (skio-aux res resh o)]
      [(ko i res) (== `(,a ,b) h) (== `(,i ,a) resh) (skio-aux res resh o)]
      [(so i res) (== `(,a ,b) h) (== `(,i ,a) resh) (skio-aux res resh o)]
      [(== `(,a ,b) i) (skio-aux a h resa) (skio-aux b h resb)
       (== `(,resa ,resb) res) 
       (== `(,c ,d) h) (== `(,res ,c) resh) (skio-aux res resh o)]
      )])))

(define (skio-aux i h o)
  (fresh (a b c d resa resb resh res)
   (conde
    [(== `(,a ,a) h) (== i a) (== i o)]
    [(varo i) (== i o)]
    [(combo i) (== i o)]
    [(io i res) (== `(,a ,b) h) (== `(,i ,a) resh) (skio-aux res resh o)]
    [(ko i res) (== `(,a ,b) h) (== `(,i ,a) resh) (skio-aux res resh o)]
    [(so i res) (== `(,a ,b) h) (== `(,i ,a) resh) (skio-aux res resh o)]
    [(== `(,a ,b) i) (skio-aux a h resa) (skio-aux b h resb)
     (== `(,resa ,resb) res) 
     (== `(,c ,d) h) (== `(,res ,c) resh) (skio-aux res resh o)]
    )))

#;(define (skio-aux i o)
  (fresh (a b resa resb res)
   (conde
    [(irredexo i) (== i o)]
    [(conde
      [(io i res) (skio-aux res o)]
      [(ko i res) (skio-aux res o)]
      [(so i res) (skio-aux res o)]
      [(== `(,a ,b) i) (skio-aux a resa) (skio-aux b resb)
       (== `(,resa ,resb) res) (=/= i res) (skio-aux res o)]
      [(== `(,a ,b) i) (skio-aux a resa) (skio-aux b resb)
       (== `(,resa ,resb) res) (== i res) (== i o)]
      )])))

(define (irredexo i)
  (fresh (a b c d e x y z)
   (conde
    [(varo i)]
    [(combo i)]
    [(== `(K ,a) i) (irredexo a)]
    [(== `(S ,a) i) (irredexo a)]
    [(== `((S ,a) ,b) i) (irredexo a) (irredexo b)]
)))
     
     
;;interpreter interface
(define (skio i o)
  (fresh (a d)
   (conde
    [(laso i a) (skio-aux a '(init init) o)])))

#;(define (skio i o)
  (fresh (a)
   (laso i a) (skio-aux a o)))
