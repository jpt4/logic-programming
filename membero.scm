(load "miniKanren-with-symbolic-constraints/mk.scm")
(load "miniKanren-with-symbolic-constraints/numbers.scm")

;m a member of l
(define (member?o m l)
  (conde
   [(== '() m) (== '() l)]
   [(fresh (a d)
      (== `(,a . ,d) l)
      (conde
       [(== m a)]
       [(=/= m a) (member?o m d)]))]))

;count occurrences of a in b - fine for run 1, loops for run >1.
(define (occurso a b n)
  (conde
   [(== '() b) (== '() n)]
   [(fresh (ab db next)
      (== `(,ab . ,db) b)
      (conde
       [(=/= a ab) (occurso a db n)]
       [(== a ab)
        (pluso next '(1) n)
        (occurso a db next)]
       ))]))

;is-a-set? predicate
(define (set?o s)
  (conde
   [(== '() s)]
   [(fresh (as ds)
      (== `(,as . ,ds) s)
      (occurso as s '(1))
      (set?o ds))]))

;element of

;m is not a member of l. Nested run* breaks relational symmetry, I believe.
(define (not-membero m l)
  (fresh (res)
    (== '() res)
    (== res (run* (p) (membero m l)))))

;a subset of b
(define (subseto a b)
  (conde
   [(== '() a)]
   [(fresh (cara cdra res)
      (== `(,cara . ,cdra) a)
      (subseto cdra b)
      (membero cara b))]))

;return intersection of sets a and b
#;(define (set-intersecto a b bagi res)
  (== (run* (p q r) (set-intersecto-aux a b bagi)
            (== p a) (== q b) (== r bagi)) res))

(define (set-intersecto a b i)
  (conde
   [(== '() a) (== '() i)]
   [(== '() b) (== '() i)]
   [(fresh (cara cdra res)
      (== `(,cara . ,cdra) a)
      (conde
       [(membero cara b) (== `(,cara . ,res) i) (set-intersecto cdra b res)
        ]
       [(set-intersecto cdra b i)
        ]))]))

;relates input grid (list of rows) to output grid, using size as a cheat
(define (sudokuo s i o)
  (fresh (lon)
    (iotao s lon)
    (sudokuo-aux lon i o)))
(define (sudokuo-aux lon i o)
  (conde
   [(== '() i) (== '() o)]
   [(fresh (cari caro nexti nexto)
      (== `(,cari . ,nexti) i) (== `(,caro . ,nexto) o)
      (set-intersecto cari caro lon)
      (sudokuo-aux lon nexti nexto))]))

;list of numbers from s to 1
(define (iotao s l)
  (fresh (res next)
    (conde
     [(== '() s) (== '() l)]
     [(pluso '(1) next s) (== `(,s . ,res) l) (iotao next res)])))

;a is a permutation of b, both sets
#|(define (permuteo a b)
  (conde
   [(== '() a) (== '() b)]
   [(fresh (cara cdra)
      (== `(,cara . ,cdra) a)
      (membero cara b)
|#      
;returns a set d of elements in a and not b. Doesn't work.
#;(define (set-diffo a b d)
  (conde
   [(== '() a) (== '() d)]
   [(== '() b) (== a d)]
   [(fresh (cara cdra carb cdrb res)
      (== `(,cara . ,cdra) a)
      (== `(,carb . ,cdrb) b)
      (conde
       [(membero cara b) (set-diffo cdra b d)]
       [(== d `(,cara . ,res)) (set-diffo cdra b res)]
))]))
