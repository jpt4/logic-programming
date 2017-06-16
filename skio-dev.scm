;;skio-dev.scm
;;utc20170616
;;jpt4
;;Relational SKI combinator calculus interpreter, development file.
;;Chez Scheme v9.4-1

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
(define (termo i)
  (conde
   [(varo i)]
   [(combo i)]
   [(fresh (a d)
     (== `(,a ,d) i)
     (termo a) (termo d))]))

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
     #;(== `((,resa ,resb) ,resc) o))]))

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

(define (skio i d o)
  (fresh (a b c resa resb resd resad resbd res exp diag)
   (conde
    [(== `(,a (,a ,b)) d) (== i a) (== i o)]
    [(conde
      [(combo i) (== `(,i ,d) resd) (skio i resd o)]
      [(varo i) (== `(,i ,d) resd) (skio i resd o)]
      [(io i res) (== `(,res ,d) resd) (skio res resd o)]
      [(ko i res) (== `(,res ,d) resd) (skio res resd o)]
      [(so i res) (== `(,res ,d) resd) (skio res resd o)]
      [(== `(,a ,b) i) (skio a d resa) (skio b d resb) 
       (== `(,resa ,resb) res) (== `(,res ,d) resd) (skio res resd o)]
      )])))

;;Diagnostics intsrumented interpreter, with historical cruft preserved.
(define (skio-diag i d o)
  (fresh (a b c resa resb resd resad resbd res exp diag)
   (conde
    [(== `(,a (,a ,b)) d) (== i a) (== i o) 
     #;(== `(stop i=,i a=,a b=,b d=,d) o)]
    [(conde
;    [(== '() i) (== i o)]
      [(combo i) (== `(,i ,d) resd) (skio-diag i resd o)]
      [(varo i) (== `(,i ,d) resd) (skio-diag i resd o)]
      [(io i res) (== `(,res ,d) resd) (skio-diag res resd o) 
       #;(skio-diag res resd diag)
       #;(== `(so-lax i=,i res=,res d=,d resd=,resd diag=,diag) o)]
      [(ko i res) (== `(,res ,d) resd) (skio-diag res resd o) 
       #;(skio-diag res resd diag)
       #;(== `(so-lax i=,i res=,res d=,d resd=,resd diag=,diag) o)]
      [(so i res) (== `(,res ,d) resd) (skio-diag res resd o) 
       #;(skio-diag res resd diag)
       #;(== `(so-lax i=,i res=,res d=,d resd=,resd diag=,diag) o)]
      [(== `(,a ,b) i) (skio-diag a d resa) (skio-diag b d resb) 
       (== `(,resa ,resb) res) (== `(,res ,d) resd) (skio-diag res resd o)
       #;(skio-diag res resd diag)
       #;(== `(pair i=,i a=,a b=,b resa=,resa resb=,resb res=,res resd=,resd d=,d diag=,diag) o)]
#;      [(== `((,a ,b) ,c) i)
       (skio-diag b res) (skio-diag `((,a ,res) ,c) o)]
#;      [(== `(,a (,b ,c)) i) 
       (skio-diag b res) (skio-diag `(,a (,b ,c)) o)]
    )])))

;;Experimental tracing instrumented interpreter
(define (skio-dt i d t o)
  (fresh (a b c resa resb resd resad resbd res exp diag)
   (conde
    [(== `(,a (,a ,b)) d) (== i a) (== d t) (== i o) 
     #;(== `(stop i=,i a=,a b=,b d=,d t=,t) o)]
    [(conde
;    [(== '() i) (== i o)]
      [(combo i) (== `(,i ,d) resd) (skio-dt i resd t o)]
      [(varo i) (== `(,i ,d) resd) (skio-dt i resd t o)]
      [(io i res) (== `(,res ,d) resd) (skio-dt res resd t o) 
       #;(skio-dt res resd t diag)
       #;(== `(io i=,i res=,res d=,d resd=,resd t=,t diag=,diag) o)]
      [(ko i res) (== `(,res ,d) resd) #;(skio-dt res resd t o) 
       (skio-dt res resd t diag)
       (== `(ko i=,i res=,res d=,d resd=,resd t=,t diag=,diag) o)]
      [(so i res) (== `(,res ,d) resd) (skio-dt res resd t o) 
       #;(skio-dt res resd t diag)
       #;(== `(so i=,i res=,res d=,d resd=,resd t=,t diag=,diag) o)]
      [(== `(,a ,b) i) (skio-dt a d t resa) (skio-dt b d t resb) 
       (== `(,resa ,resb) res) (== `(,res ,d) resd) #;(skio-dt res resd t o)
       #;(skio-dt res resd t diag)
       (== `(pair i=,i a=,a b=,b resa=,resa resb=,resb res=,res resd=,resd d=,d t=,t diag=,diag) o)]
#;      [(== `((,a ,b) ,c) i)
       (skio-dt b res) (skio-dt `((,a ,res) ,c) o)]
#;      [(== `(,a (,b ,c)) i) 
       (skio-dt b res) (skio-dt `(,a (,b ,c)) o)]
    )])))
