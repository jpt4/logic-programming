;;skio-lax.scm
;;utc20170616
;;jpt4
;;Experiment (unsuccessful) with direct evaluation of non-fully parenthesized
;;SKI terms/expressions
;;Chez Scheme v9.4-1

(load "skio.scm")

;;Terms need only be proximal, not complete
(define (termo-lax i)
  (conde
   [(varo i)]
   [(combo i)]
   [(fresh (a d)
     (== `(,a ,d) i) (termo-lax a) (termo-lax d))]
   [(fresh (a d)
     (== `(,a . ,d) i) (termo-lax a) (termo-lax d))]))

(define (build-expo a d o)
  (fresh (ad dd b)
   (conde
    [(== '() d) #;(== 'not-pair-null o) (== `(,a . ,d) o)]
    [(varo d) #;(== 'varo o) (== `(,a ,d) o)]
    [(combo d) (== 'combo o) (== `(,a ,d) o)]
    [(== `(,ad . ,dd) d) (=/= '() dd) #;(== 'list o) (== `(,a . ,d) o)])))

(define (io-lax i o)
  (fresh (x d)
   (conde
    [(== '(I) i) (== 'I o)]
    [(== `(I ,x) i) (== x o)]
    [(== `(I ,x . ,d) i) (=/= '() d) (== `(,x . ,d) o)])))
(define (ko-lax i o)
  (fresh (x y d)
   (conde
    [(== '(K) i) (== 'K o)]
    [(== `(K ,x) i) (== i o)]
    [(== `(K ,x ,y) i) (== x o)]
    [(== `(K ,x ,y . ,d) i) (=/= '() d) (== `(,x . ,d) o)])))
(define (so-lax i o)
  (fresh (x y z d)
   (conde
    [(== '(S) i) (== 'S o) #;(== '(S) o)]
    [(== `(S ,x) i) (== i o) #;(== '(S x) o)]
    [(== `(S ,x ,y) i) (== i o) #;(== '(S x y) o)]
    [(== `(S ,x ,y ,z) i) (== `(,x ,z (,y ,z)) o) #;(== `(S x=,x y=,y z=,z) o)]
    [(== `(S ,x ,y ,z . ,d) i) (=/= '() d) (== `(,x ,z (,y ,z) . ,d) o)
     #;(== `(S x=,x y=,y z=,z d=,d) o)])))

(define (skio-lax i o)
  (fresh (a b c d aa da x y z res exp diag)
   (conde
    [(== '() i) (== i o)]
    [(combo i) (== i o)]
    [(varo i) (== i o)]
    [(io-lax i res) (skio-lax res o)]
    [(ko-lax i res) (skio-lax res o)]
    [(so-lax i res) (skio-lax res o)]
    [(== `(,a . ,b) i) (varo a)
     (skio-lax b res) (build-expo a res o) 
     #;(skio-lax b o)
     #;(build-expo a res diag) #;(== `(a=,a b=,b res=,res diag=,diag) o)]
    #;[(== `(,a . ,b) i) (combo a) (skio-lax b res) (build-expo a res o)
     (build-expo a res diag) (== `(a=,a b=,b res=,res diag=,diag) o)]
    [(== `(,a . ,d) i) (== `(,aa . ,da) a) 
     (skio-lax a res) (build-expo res d exp) (skio-lax exp o)]
    )))
