;20130123 JPTIV
;Rev. 20131118
;Rev. 20131122

(load "mk.scm")
(load "minikanren.scm")

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

;Explosive nand
(define enand-o
  (lambda (a b c)
    (fresh (d e f)
      (and-o a b d)
      (enot-o d c))))

;Explosive nor
(define enor-o
  (lambda (a b c)
    (fresh (d e f)
      (or-o a b d)
      (enot-o d c))))

;Extension to 4 field
;(#f b c d) -> (#f d b c)

#|
(run 1 (q) (fresh (a b c d e f g h)
               (== (,a ,b ,c ,d) (1 2 3 4))
               ;(== (,e ,f ,g ,h) 
               (3f-o a b c e f g)
               (3f-o)
               (== `(a=,a b=,b c=,c d=,d 
                      e=,e f=,f g=,g h=,h)
                 q)))
|#

;Extension to n-field
;(#f a1 a2 ... an) -> (#f an a1 a2 ... an-1)

(define nf-o
  (lambda (i o)
    (conde
      [(fresh (a)
         (caro i a)
         (=/= a #f)
         (== i o))]
      [(fresh (a d res)
         (conso a d i)
         (== a #f)
         (nrotate-o d '(1) res)
         (== `(,a . ,res) o))])))

(define nrotate-o
  (lambda (i n o)
    (conde
      [(== '() n) (== i o)]
      [(fresh (a d m res)
         (=/= '() n)
         (conso a d i)
         (appendo d `(,a) res)
         (sub1-o n m)
         (nrotate-o res m o))])))

(define sub1-o
  (lambda (i o)
    (minuso i '(1) o)))

