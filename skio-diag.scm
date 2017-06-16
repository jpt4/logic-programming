;;skio-diag.scm utc20170616 jpt4

;;Diagnostics instrumented interpreter core
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
