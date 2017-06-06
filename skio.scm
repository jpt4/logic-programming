;;skio.scm
;;utc20170530
;;jpt4
;;Relational SKI combinator calculus interpreter

(load "miniKanren-with-symbolic-constraints/mk.scm")

(define (varo i)
  (conde
   [(symbolo i)
    (=/= 'S i) (=/= 'K i) (=/= 'I i)]))
          
;;Terms need only be proximal, not complete
(define (termo i)
  (conde
   [(varo i)]
   [(== 'S i)]
   [(== 'K i)]
   [(== 'I i)]
   [(fresh (a d)
     (== `(,a ,d) i) (=/= '() d)
     (termo a) (termo d))]))
;;Irreducible terms
(define (ground-termo i)
  (fresh (x y z)
   (conde
    [(termo i) 
     (=/= `(I ,x) i) (=/= `((K ,x) ,y) i) (=/= `(((S ,x) ,y) ,z) i)])))
;;Irreducible expressions
(define (ground-expo i)
  (fresh (a b c x y z)
   (conde
    [(ground-termo i)]
    [(== `(,a ,b) i) (ground-termo a) (ground-termo b)]
    [(== `((,a ,b) ,c) i) 
     (ground-termo a) (ground-termo b) (ground-termo c)])))

(define (io i o)
  (fresh (x)
         (== `(I ,x) i)
         (== x o)))
(define (ko i o)
  (fresh (x y)
         (== `((K ,x) ,y) i)
         (== x o)))
(define (so i o)
  (fresh (x y z)
         (== `(((S ,x) ,y) ,z) i)
         (== `((,x ,z) (,y ,z)) o)))

(define (skio i o)
  (fresh (res)
   (conde
    [(base-argo i) (== i o)]
    [(io i res) (skio res o)]
    [(ko i res) (skio res o)]
    [(so i res) (skio res o)]
   )))

(define (laso i o)
  (conde
   [(varo i) (== i o)]
   [(termo i) (== i o)]
   [(fresh (a ad dd resa resad resdd)
     (== `(,a ,ad ,dd) i) 
     (laso a resa) (laso ad resad) (laso dd resdd)
     (== `((,resa ,resad) ,resdd) o))]
   [(fresh (a ad dd add ddd resa resad resdd)
     (== `(,a ,ad . ,dd) i) (=/= '() dd)
     (laso a resa) (laso ad resad) (laso dd resdd)
     (== `((,resa ,resad) ,resdd) o))]))

#|Why?
> (run 1 (p q r) (=/= `(,p . ,q) r))
(((_.0 _.1 _.2) (=/= ((_.2 (_.0 . _.1))))))
> (run 1 (r) (fresh (p q) (=/= `(,p . ,q) r)))
(_.0)
> (run 1 (a b c) (fresh (p q r) (=/= `(,p . ,q) r) (== p a) (== q b) (== r c)))
(((_.0 _.1 _.2) (=/= ((_.2 (_.0 . _.1))))))
> (run 1 (a b c) (fresh (p q r) (=/= `(,p . ,q) r) (== r c)))
((_.0 _.1 _.2))
> (run 1 (a b c) (fresh (p q r) (=/= `(,p . ,q) r) (== r c) (== r '(1 2))))
((_.0 _.1 (1 2)))
> (run 1 (a b c) (fresh (p q r) (=/= `(,p . ,q) r) (== p a) (== q b) (== r c) 
                        (== r '(1 2))))
(((_.0 _.1 (1 2)) (=/= ((_.0 1) (_.1 (2))))))
|#
   
