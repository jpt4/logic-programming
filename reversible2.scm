;20130123 JPTIV
;Rev. 20131118

(load "mk.scm")
;(load "minikanren.scm")

;Relational 3-field Fredkin gate. Controlled swap semantics. Preserves
;argument positions if first field is #f. Swaps last two otherwise.
(define 3f-o
  (lambda (a b c d e f)
    (conde
      [(=/= a #f) (== `(,a ,b ,c) `(,d ,f ,e))]
      [(== a #f) (== `(,a ,b ,c) `(,d ,e ,f))])))

;Simulated and
(define and-o
  (lambda (a b c)
    (fresh (d e f)
      (3f-o a a b d e f)
      (== c e))))

;Simulated or
(define or-o
  (lambda (a b c)
    (fresh (d e f)
      (3f-o a b a d e f)
      (== c e))))

;Simulated not, b ranges over {#t #f}.
(define not-o
  (lambda (a b)
    (fresh (d e f)
      (3f-o a #t #f d e f)
      (== b e))))

;Simulated nor, demonstrates classical logical completeness of {Friedkin gates with #t/#f constants}.
(define nor-o
  (lambda (a b c)
    (fresh (d e f g h i)
      (3f-o a b a d e f)
      (3f-o e #t #f g h i)
      (== c h))))

;Simulated nand, demonstrates classical logical completeness of {Friedkin gates with #t/#f constants}.
(define nand-o
  (lambda (a b c)
    (fresh (d e f g h i)
      (3f-o a a b d e f)
      (3f-o e #t #f g h i)
      (== c h))))

;Explosive not, partitions the expression domain into #f and {everything else}
(define enot-o
  (lambda (a b)
    (fresh (d e f g h i j k l)
      (3f-o a g #f d e f)
      (3f-o g g h i #t j)
      (== b e))))
#|
(define enot-o
  (lambda (a b)
    (fresh (c d e f g h)
      (3f-o a c #f d b e)
      (3-friedkino c c #t g #t h))))
|#

;Explosive nand
(define enand-o
  (lambda (a b c)
    (fresh (d e f)
      (and-o a b d)
      (enot-o d c))))

#|
(define enand-o
  (lambda (a b c)
    (fresh (e f g h i j k)
      (3-friedkino a a b d c e)
      (3-friedkino c f #f h h i)
      (3-friedkino h h #t j #t k))))
|#

;Explosive nor
(define enor-o
  (lambda (a b c)
    (fresh (d e f)
      (or-o a b d)
      (enot-o d c))))

#|
(define e-noro
  (lambda (a b c)
    (fresh (e f g h i j k)
      (3-friedkino a b a b d c e)
      (3-friedkino c f #f g h i)
      (3-friedkino h h #t j #t k))))
|#

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
