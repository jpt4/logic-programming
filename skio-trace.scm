;;skio-trace.scm utc20170616 jpt4

;;Experimental tracing instrumented interpreter core
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
