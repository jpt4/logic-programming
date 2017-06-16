;;skio-notes.scm
;;utc20170616
;;jpt4
;;skio.scm notes
;;Chez Scheme v9.4-1

;;success with strict-skio
#|
> (run 1 (o) (strict-skio '(((S (K a)) ((S I) I)) b) 'init o))
((a (b b)))
> (run 2 (o) (strict-skio '(((S (K a)) ((S I) I)) b) 'init o))
((a (b b)) (a (b b)))
> (run 5 (o) (strict-skio '(((S (K a)) ((S I) I)) b) 'init o))
((a (b b)) (a (b b)) (a (b b)) (a (b b)) (a (b b)))
> (run 1 (o) (strict-skio '(((S (K (S I))) a) b) 'init o))
(((S I) (a b)))
> (run 1 (o) (strict-skio '((((S (K (S I))) K) a) b) 'init o))
((b a))

> (run 1 (o) (strict-skio '(a (a (a b))) 'init o))
((a (a (a b))))
> (run 1 (o) (strict-skio '(a (a (K b))) 'init o))
((a (a (K b))))
> (run 1 (o) (strict-skio '((((a b) c) d) e) 'init o))
(((((a b) c) d) e))
> (run 1 (o) (strict-skio '((((K b) c) d) e) 'init o))
(((b d) e))
> (run 1 (o) (strict-skio '((((K b) c) (K d) e)) 'init o))
()
> (run 1 (o) (termo '((((K b) c) (K d) e))))
()
> (run 1 (o) (termo '((((K b) c) ((K d) e)))))
()
> (run 1 (o) (termo '(((K b) c) ((K d) e))))
(_.0)
> (run 1 (o) (strict-skio '(((K b) c) ((K d) e)) 'init o))
((b d))
>

|#

;irreducible
#;(define (no-redexo i o)
  (fresh (a1 a2 a3 b x1 x2 x3 y2 y3 z3)
   (conde
    [(termo i) (=/= `(,a1 ,x1) i) (== 'I a1) (=/= `((,a2 ,x2) ,y2) i) (== 'K a2) 
     (== `(a1=,a1 a2=,a2 a3=,a3 x1=,x1 x2=,x2 x3=,x3 y2=,y2 y3=,y3 z3=,z3) o)]
    #;[(termo i) (== `(,a ,x) i) (=/= `((,a ,x) ,y) i) (=/= `(((,a ,x) ,y) ,z) i)
     (=/= 'I a) (== `(a=,a x=,x) o)]
    #;[(termo i) (== `((,a ,x) ,y) i) (=/= `(,a ,x) i) (=/= `(((,a ,x) ,y) ,z) i)
     (=/= 'K a) (== `(a=,a x=,x y=,y) o)]
    #;[(termo i) (== `(((,a ,x) ,y) ,z) i) (=/= `((,a ,x) ,y) i) (=/= `(,a ,x) i)
     (=/= 'S a) (== `(a=,a x=,x y=,y z=,z) o)]
    #;[(termo i) (== `(,a ,b) i) (no-redexo a) (no-redexo b)])))


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
