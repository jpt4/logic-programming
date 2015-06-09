;20130123 JPTIV

(load "mk.scm")
;(load "minikanren.scm")

;Relational 3-field Friedkin gate. Evaluates each argument, swaps the last two iff the first is #f.
(define 3-friedkino
  (lambda (a b c d e f)
    (conde
      [(=/= a #f) (== `(,a ,b ,c) `(,d ,f ,e))]
      [(== a #f) (== `(,a ,b ,c) `(,d ,e ,f))])))

;Simulated and
(define ando
  (lambda (a b c)
    (fresh (d e f)
      (3-friedkino a a b d e f)
      (== c e))))

;Simulated or
(define oro
  (lambda (a b c)
    (fresh (d e f)
      (3-friedkino a b a d e f)
      (== c e))))

;Simulated not, b ranges over {#t #f}.
(define noto
  (lambda (a b)
    (fresh (d e f)
      (3-friedkino a #t #f d e f)
      (== b e))))

;Simulated nor, demonstrates classical logical completeness of {Friedkin gates with #t/#f constants}.
(define noro
  (lambda (a b c)
    (fresh (d e f g h i)
      (3-friedkino a b a d e f)
      (3-friedkino e #t #f g h i)
      (== c h))))

;Simulated nand, demonstrates classical logical completeness of {Friedkin gates with #t/#f constants}.
(define nando
  (lambda (a b c)
    (fresh (d e f g h i)
      (3-friedkino a a b d e f)
      (3-friedkino e #t #f g h i)
      (== c h))))

;Explosive not, partitions the expression domain into #f and {everything else}
(define e-noto
  (lambda (a b)
    (fresh (c d e f g h)
      (3-friedkino a c #f d b e)
      (3-friedkino c c #t g #t h))))

;Explosive nand
(define e-nando
  (lambda (a b c)
    (fresh (e f g h i j k)
      (3-friedkino a a b d c e)
      (3-friedkino c f #f h h i)
      (3-friedkino h h #t j #t k))))

;Explosive nor
(define e-noro
  (lambda (a b c)
    (fresh (e f g h i j k)
      (3-friedkino a b a b d c e)
      (3-friedkino c f #f g h i)
      (3-friedkino h h #t j #t k))))


#|
Test subjects

;Explosive relational not, b ranges over {everything}
(define e-noto
  (lambda (a b)
    (fresh (c d e f)
      (3-friedkino a c #f d e f)
      (== b e))))

(define 3-friedkino2
  (lambda (a b c d e f)
    (conde
      [(=/= a #f) (== `(,a ,b ,c) `(,d ,f ,e))]
      [(=/= a #t) (== `(,a ,b ,c) `(,d ,e ,f))])))

(define 3-friedkino2a
  (lambda (a b c d e f)
    (conda
      [(=/= a #f) (== `(,a ,b ,c) `(,d ,f ,e))]
      [(=/= a #t) (== `(,a ,b ,c) `(,d ,e ,f))])))

(define 3-friedkino3
  (lambda (a b c d e f)
    (conde
      [(=/= a #f) (== `(,a ,b ,c) `(,d ,f ,e))]
      [(== a #f) (== `(,a ,b ,c) `(,d ,e ,f))])))
|#
