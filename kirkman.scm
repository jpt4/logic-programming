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


(define (crosspro m n o)
  (conde
    [(nullo m) (nullo n) succeed]
    [(fresh (ma md na nd res)
       (== `(,ma . ,md) m) (== `(,na . ,nd) n)
       (crosspro-auxo ma n res)
			 |#

(define (crosspro-auxo s ls t o)
  (conde
    [(nullo ls) (== t o)]
    [(fresh (lsa lsd res)
       (== `(,lsa . ,lsd) ls)
       (==  `((,s . ,lsa) . ,t) res)
       (crosspro-auxo s lsd res o))]))
  
    
