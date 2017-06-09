;;skio.scm
;;utc20170530
;;jpt4
;;Relational SKI combinator calculus interpreter

(load "miniKanren-with-symbolic-constraints/mk.scm")

(define (varo i)
  (conde
   [(symbolo i)
    (=/= 'I i) (=/= 'K i) (=/= 'S i)]))

(define (combo i)
  (conde
   [(== 'I i)]
   [(== 'K i)]
   [(== 'S i)]))

;;Terms need only be proximal, not complete
(define (termo i)
  (conde
   [(varo i)]
   [(combo i)]
   [(fresh (a d)
     (== `(,a ,d) i) (=/= '() d)
     (termo a) (termo d))]))
#;(define (term?o i o)
  (conde
   [(termo i) (== i o)]))
(define (build-expo-min a d o)
  (fresh (ad dd b)
   (conde
    [(== '() d) #;(== 'not-pair-null o) (== `(,a . ,d) o)]
    [(varo d) #;(== 'varo o) (== `(,a ,d) o)]
    [(combo d) (== 'combo o) (== `(,a ,d) o)]
    [(== `(,ad . ,dd) d) (=/= '() dd) #;(== 'list o) (== `(,a . ,d) o)])))

(define (io i o)
  (fresh (x d)
   (conde
    [(== '(I) i) (== 'I o)]
    [(== `(I ,x) i) (== x o)]
    [(== `(I ,x . ,d) i) (=/= '() d) (== `(,x . ,d) o)])))
(define (ko i o)
  (fresh (x y d)
   (conde
    [(== '(K) i) (== 'K o)]
    [(== `(K ,x) i) (== i o)]
    [(== `(K ,x ,y) i) (== x o)]
    [(== `(K ,x ,y . ,d) i) (=/= '() d) (== `(,x . ,d) o)])))
(define (so i o)
  (fresh (x y z d)
   (conde
    [(== '(S) i) (== 'S o) #;(== '(S) o)]
    [(== `(S ,x) i) (== i o) #;(== '(S x) o)]
    [(== `(S ,x ,y) i) (== i o) #;(== '(S x y) o)]
    [(== `(S ,x ,y ,z) i) (== `(,x ,z (,y ,z)) o) #;(== `(S x=,x y=,y z=,z) o)]
    [(== `(S ,x ,y ,z . ,d) i) (=/= '() d) (== `(,x ,z (,y ,z) . ,d) o)
     #;(== `(S x=,x y=,y z=,z d=,d) o)])))

(define (skio i o)
  (fresh (a b c d aa da x y z res exp diag)
   (conde
    [(== '() i) (== i o)]
    [(combo i) (== i o)]
    [(varo i) (== i o)]
    [(io i res) (skio res o)]
    [(ko i res) (skio res o)]
    [(so i res) (skio res o)]
    [(== `(,a . ,b) i) (=/= `(,aa . ,da) a) (=/= 'I a) (=/= 'K a) (=/= 'S a)
     (skio b res) #;(build-expo a res o) 
     #;(skio b o)
     #;(build-expo a res diag) #;(== `(a=,a b=,b res=,res diag=,diag) o)]
    [(== `(,a . ,d) i) (== `(,aa . ,da) a) 
     (skio a res) (build-expo res d exp) (skio exp o)]
    )))

;;Irreducible terms
#;(define (ground-termo i o)
  (fresh (a x y z)
   (conde
    [(== 'I i) (== i o)]
    [(== 'K i) (== i o)]
    [(== 'S i) (== i o)]
    [(== `(,a ,x) i) (=/= 'I a) (termo i) (== i o)]
    [(== `((,a ,x) ,y) i) (=/= 'K a) (termo i) (== i o)]
    [(== `(((,a ,x) ,y) ,z) i) (=/= 'S a) (termo i) (== i o)])))
#;(define (ground-term?o i)
  (fresh (o)
   (ground-termo i o)))
;;Irreducible expressions
#;(define (ground-expo i)
  (fresh (a b c x y z)
   (conde
    [(ground-termo i)]
    [(== `(,a ,b) i) (ground-termo a) (ground-termo b)]
    [(== `((,a ,b) ,c) i) 
     (ground-termo a) (ground-termo b) (ground-termo c)])))

#;(define (irredexo i)
  (fresh (a d x y z)
   (conde
    [(varo i)]
    [(== 'I i)]
    [(== 'K i)]
    [(== 'S i)]
    [(== '(I) i)]
    [(== '(K) i)]
    [(== '(S) i)]
    [(== `(,a I) i) (varo a)]
    [(== `(K ,a) i) (varo a)]
    [(== `(,a K) i) (varo a)]
    [(== `(,a I) i) (varo a)]    
    [(== `(S ,a) i) (varo a)])))


#;(define (laso i o)
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
***
> (run* (q) (fresh (x y z) (conde
                             [(== '(((S x) y) z) q) (=/= '(((S ,x) ,y) ,z) q)])))
((((S x) y) z))
> (run* (q) (fresh (a x y z) (conde
                             [(== '(((S x) y) z) q) (=/= `(((S ,x) ,y) ,z) q)])))
((((S x) y) z))
> (run* (q) (fresh (a x y z) (conde
                             [(== '(((S x) y) z) q) (== `(((,a ,x) ,y) ,z) q) (=/= a 'S) (=/= `(((S ,x) ,y) ,z) q)])))
()
> (run* (q) (fresh (a x y z) (conde
                             [(== '(((S x) y) z) q) (== `(((,a ,x) ,y) ,z) q) (=/= a 'S)])))
()
> (run* (q) (fresh (a x y z) (conde
                             [(termo q) (== '(((S x) y) z) q) (== `(((,a ,x) ,y) ,z) q) (=/= a 'S)])))
^Cbreak> q

> (run* (q) (fresh (a x y z) (conde
                             [(== '(((S x) y) z) q) (== `(((,a ,x) ,y) ,z) q) (=/= a 'S) (termo q)])))
()
>
|#
   
;;Lessons
#|
(run* (a b p q) (build-expo a b p) (build-expo-min a b q) (=/= p q))
(define (build-expo a d o)
  (fresh (ad dd b)
   (conde
    [(== `(,ad . ,dd) d) (== '() dd) #;(== 'list-1 o) (== `(,a . ,d) o)]
    [(== `(,ad . ,dd) d) (=/= '() dd) #;(== 'list o) (== `(,a . ,d) o)]
    [(=/= `(,ad . ,dd) d) (== '() d) #;(== 'not-pair-null o) (== `(,a . ,d) o)]
    [(varo d) #;(== 'not-pair-not-null o) (== `(,a ,d) o)]
    [(combo d) #;(== 'not-pair-not-null o) (== `(,a ,d) o)])))
(define (build-expo-min a d o)
  (fresh (ad dd b)
   (conde
    ;[#;(== `(,ad . ,dd) d) #;(== '() dd) (== 'list-1 o) #;(== `(,a . ,d) o)]
    [(== `(,ad . ,dd) d) (=/= '() dd) #;(== 'list o) (== `(,a . ,d) o)]
    [#;(=/= `(,ad . ,dd) d) (== '() d) #;(== 'not-pair-null o) (== `(,a . ,d) o)]
    [(varo d) #;(== 'varo o) (== `(,a ,d) o)]
    [(combo d) (== 'combo o) (== `(,a ,d) o)])))
|#