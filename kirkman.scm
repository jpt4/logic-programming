;;  kirkman.scm  jpt4  UTC20150628

(load "miniKanren-with-symbolic-constraints/mk.scm")
(load "miniKanren-with-symbolic-constraints/numbers.scm")

;;  produce set B of subsets of size 3, from set P of size v >= 3, such that
;;  no (i,j) pair /element-of/ PxP is present in more than one subset.
#|
(define (kirkmano p b)
  (fresh (ps cr)
    (crosspro p p cr)
    (== ps cr) 
(conde
[])))
|#

(define (crosspro m n t o)
  (conde
    [(== '() m) (=/= '() n) (== t o)]
    [(fresh (ma md na nd rest reso)
       (== `(,ma . ,md) m) (== `(,na . ,nd) n)
       (crosspro-auxo ma n rest reso)
       (crosspro md n rest reso))]))


(define (crosspro-auxo s ls t o)
  (conde
    [(=/= '() ls) (=/= '() t) (== t o)]
    [(fresh (sa lsa lsd res)
       (== `(,sa) s)
       (== `(,lsa . ,lsd) ls)
       (==  `((,sa . ,lsa) . ,t) res)
       (crosspro-auxo s lsd res o))]))
  
