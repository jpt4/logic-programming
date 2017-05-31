;;skio.scm
;;utc20170530
;;jpt4
;;Relational SKI combinator calculus interpreter

(load "miniKanren-with-symbolic-constraints/mk.scm")

(define (base-argo i)
  (conde
   [(symbolo i)
    (=/= 'S i) (=/= 'K i) (=/= 'I i)]))
          
;;Terms need only be proximal, not complete
(define (ski-termo i)
  (conde
   [(== 'S i)]
   [(== 'K i)]
   [(== 'I i)]
   [(fresh (a d)
     (== `(,a ,d) i)
     (ski-termo a) (ski-termo d))]))

(define (laso i o)
  (conde
   [(ski-termo i) (== i o)]
   [(fresh (a ad dd resa resad resdd)
     (== `(,a ,ad . ,dd) i) (=/= '() dd)
     (laso a resa) (laso ad resad) (laso dd resdd)
     (== `((,resa ,resad) . ,resdd) o))]))

;;XXX Require an expression builder of some sort to convert terms + args to 
;;evaluable expressions, determine if possible.

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
   
