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
    [(fresh (ma md na nd rest reso freso)
       (== `(,ma . ,md) m) (== `(,na . ,nd) n)
       (crosspro-auxo `(,ma) n rest)
       (== `(,rest . ,reso) freso)
       (== freso o)
;       (unwrapo freso o)
       (crosspro md n reso))]))

(define (crosspro-auxo s ls o)
  (conde
    [(=/= '() s) (== '() ls) (== '() o)]
    [(fresh (sa lsa lsd res)
       (== `(,sa) s)
       (== `(,lsa . ,lsd) ls)
       (==  `((,sa . ,lsa) . ,res) o)
       (crosspro-auxo s lsd res))]))
  
;;  list of lists of x - ((x x) (x x))
;;  -> list of x - (x x x x)
(define (unwrapo ls o)
  (conde
    [(null?o ls) (null?o o)]
    [(fresh (a aa ad d next res)
       (caro ls a) (cdro ls d) 
       (== `(,aa . ,ad) a) (== `(,aa . ,res) o)
       (== `(,ad . ,d) next) (unwrapo next res))]))

(define (diag-unwrapo ls o)
  (conde
    [(null?o ls) (null?o o)]
    [(fresh (a aa ad d next res)
       (caro ls a) (cdro ls d) 
       (== `(,aa . ,ad) a) (== `(,aa . ,res) next)
       (== `(,aa ,ad . ,d) o))]))

;test sgit edit - does this push?
#|
    [(fresh (a d resa resd)
       (== `(,a . ,d) ls) (pair?o a)
       (== `(,resa . ,resd) o)       
       (unwrapo a resa) (unwrapo d resd))]
    [(fresh (a d)
       (== `(,a . ,d) ls)
       (pair?o a) (pair?o d)
       (== `(,a . ,d) o))]))
|#