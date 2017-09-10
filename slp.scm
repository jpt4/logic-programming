;; slp.scm
;; UTC20170909
;; jpt4
;; sentential logic prover, from Robert Katz, 1994,
;; "Classical Sentential Logic Based on Logical Equivalence"
;; Chez Scheme v9.4.1

(load "miniKanren-with-symbolic-constraints/mk.scm")
(load "miniKanren-with-symbolic-constraints/numbers.scm")

(define (simple-statement ss)
  (conde
   [(symbolo ss) (=/= ss '~) (=/= ss '&) (=/= ss '//)])
  )

(define (mk-not s c) (== `(~ ,s) c))
(define (mk-and s1 s2 c) (== `(& ,s1 ,s2) c))
(define (mk-or s1 s2 c) (== `(// ,s1 ,s2) c))

(define (any-statement s)
  (conde
   [(simple-statement s)]
   [(compound-statement s)]))

(define (compound-statement c)
  (fresh (s1 s2)
         (conde
          [(mk-not s1 c) (any-statement s1)]
          [(mk-and s1 s2 c) (any-statement s1) (any-statement s2)]
          [(mk-or s1 s2 c) (any-statement s1) (any-statement s2)]
)))

;; Substitution, Symmetry, and Extended Transitivity Principles
;; are satisfied by the semantics of ==.
;;^^may be wrong, irrelevant

;Symmetry
(define (sym r i o) ;rule r does not need to be passed quoted. (sym dneg 'x q) => (~ (~ x))
  (r o i))

;; Eight Assumed Equivalence Rules

;Double Negation
(define (dneg i o)
  (conde
   [(fresh (x)
           (any-statement x)
           (== `(~ (~ ,x)) i)
           (== x o))]))
;Idempotency
(define (idem i o)
  (conde
   [(fresh (x)
           (any-statement x)
           (== `(& ,x ,x) i)
           (== x o))]))
;Commutativity
(define (comm i o)
  (conde
   [(fresh (x y)
           (any-statement x) (any-statement y)
           (== `(& ,x ,y) i)
           (== `(& ,y ,x) o))]))
;Associativity
(define (assoc i o)
  (conde
   [(fresh (x y z)
           (any-statement x) (any-statement y) (any-statement z)
           (== `(& ,x (& ,y ,z)) i)
           (== `(& (& ,x ,y) ,z) o))]))
;Absorption
(define (absorp i o)
  (conde
   [(fresh (x y)
           (any-statement x) (any-statement y)
           (== `(& ,x (// ,x ,y)) i)
           (== x o))]))
;Distributivity
(define (distr i o)
  (conde
   [(fresh (x y z)
           (any-statement x) (any-statement y) (any-statement z)
           (== `(& ,x (// ,y ,z)) i)
           (== `(// (& ,x ,y) (& ,y ,z)) o))]))
;DeMorgan's Rule
(define (dem i o)
  (conde
   [(fresh (x y)
           (any-statement x) (any-statement y)
           (== `(~ (& ,x ,y)) i)
           (== `(& (~ ,x) (~ ,y)) o))]))
;Dichotomy
(define (dichot i o)
  (conde
   [(fresh (x y)
           (any-statement x) (any-statement y)
           (== `(// ,x (~ ,x)) i)
           (== `(// ,y (~ ,y)) o))]))

;; Syntactically mark statements as equivalent.
(define (mk-eqv i o)
  (conde
   [(eqvo i o) `(eqv i o)]))

(define (eqvo i t o)
  (fresh (x y)
         (conde
          [(== i o)]
          [(dneg i x) ]
          )))

(define (mapo rel ls o)
  (fresh (a d res acc)
         (conde
          [(== '() ls) (== '() o)]
          [(== `(,a . ,d) ls) (rel a res) 
           (== `(,res . ,acc) o)
           (mapo rel d acc)]
          )))
           

#;(define (thunko i o)
  (== (lambda () i) o))



