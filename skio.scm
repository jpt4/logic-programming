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
(define (skio-aux i d o)
  (fresh (a b c resa resb resd resad resbd res exp diag)
   (conde
    [(== `(,a (,a ,b)) d) (== i a) (== i o)]
    [(conde
      [(combo i) (== `(,i ,d) resd) (skio-aux i resd o)]
      [(varo i) (== `(,i ,d) resd) (skio-aux i resd o)]
      [(io i res) (== `(,res ,d) resd) (skio-aux res resd o)]
      [(ko i res) (== `(,res ,d) resd) (skio-aux res resd o)]
      [(so i res) (== `(,res ,d) resd) (skio-aux res resd o)]
      [(== `(,a ,b) i) (skio-aux a d resa) (skio-aux b d resb) 
       (== `(,resa ,resb) res) (== `(,res ,d) resd) (skio-aux res resd o)]
      )])))

;;interpreter interface
(define (skio i o)
  (fresh (a d)
   (conde
    [(laso i a) (skio-aux a d o)])))
