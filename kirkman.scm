;;  kirkman.scm  jpt4  UTC20150628
;;  Kirkman's Schoolgirl Puzzle

(load "miniKanren-with-symbolic-constraints/mk.scm")
(load "miniKanren-with-symbolic-constraints/numbers.scm")
(load "prelude.scm")

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

(define (crosspro m n o)
  (conde
    [(== '() m) (=/= '() n) (== '() o)]
    [(fresh (ma md na nd rest reso)
       (== `(,ma . ,md) m) (== `(,na . ,nd) n)
       (crosspro-auxo `(,ma) n rest)
       (== `(,rest . ,reso) freso)
       (flatteno freso o)
       (crosspro md n reso))]))

(define (crosspro-auxo s ls o)
  (conde
    [(=/= '() s) (== '() ls) (== '() o)]
    [(fresh (sa lsa lsd res)
       (== `(,sa) s)
       (== `(,lsa . ,lsd) ls)
       (==  `((,sa . ,lsa) . ,res) o)
       (crosspro-auxo s lsd res))]))
  

