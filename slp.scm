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
;rule r does not need to be passed quoted. (sym dneg 'x q) => (~ (~ x))
(define (sym r i o) 
  (r o i))

;; Eight Assumed Equivalence Rules and Duals

;Double Negation
(define (dneg i o)
  (conde
   [(fresh (x)
           (any-statement x)
           (== `(~ (~ ,x)) i)
           (== x o))]))
(define (du-dneg i o) (sym dneg i o))
;Idempotency
(define (idem i o)
  (conde
   [(fresh (x)
           (any-statement x)
           (== `(& ,x ,x) i)
           (== x o))]))
(define (du-idem i o) (sym idem i o))
;Commutativity
(define (comm i o)
  (conde
   [(fresh (x y)
           (any-statement x) (any-statement y)
           (== `(& ,x ,y) i)
           (== `(& ,y ,x) o))]))
(define (du-comm i o) (sym comm i o))
;Associativity
(define (assoc i o)
  (conde
   [(fresh (x y z)
           (any-statement x) (any-statement y) (any-statement z)
           (== `(& ,x (& ,y ,z)) i)
           (== `(& (& ,x ,y) ,z) o))]))
(define (du-assoc i o) (sym assoc i o))
;Absorption
(define (absorp i o)
  (conde
   [(fresh (x y)
           (any-statement x) (any-statement y)
           (== `(& ,x (// ,x ,y)) i)
           (== x o))]))
(define (du-absorp i o) (sym absorp i o))
;Distributivity
(define (distr i o)
  (conde
   [(fresh (x y z)
           (any-statement x) (any-statement y) (any-statement z)
           (== `(& ,x (// ,y ,z)) i)
           (== `(// (& ,x ,y) (& ,y ,z)) o))]))
(define (du-distr i o) (sym distr i o))
;DeMorgan's Rule
(define (dem i o)
  (conde
   [(fresh (x y)
           (any-statement x) (any-statement y)
           (== `(~ (& ,x ,y)) i)
           (== `(& (~ ,x) (~ ,y)) o))]))
(define (du-dem i o) (sym dem i o))
;Dichotomy
(define (dichot i o)
  (conde
   [(fresh (x y)
           (any-statement x) (any-statement y)
           (== `(// ,x (~ ,x)) i)
           (== `(// ,y (~ ,y)) o))]))
(define (du-dichot i o) (sym dichot i o))

;; Syntactically mark statements as equivalent.
(define (mk-eqv i o)
  (conde
   [(eqvo i o) `(eqv i o)]))

(define (eqvo i t o)
  (fresh (x y new-exp res-sub-t sub-t sub-o
            s1 s2 res-sub-t-1 res-sub-t-2 sub-t-1 sub-t-2 sub-o-1 sub-o-2)
         (conde
          ;trivial result
          [(== i o) (== '() t)]
          ;simple statement grounding
          [(simple-statement i) (== `((,i simp)) t) (== i o)]
          ;eight equivalence rules - contraction
          [(dneg i x) (== `((,x dneg) . ,y) t) 
           #;(rule-result-cycle-check x t) (eqvo x y o)]
          [(idem i x) (== `((,x idem) . ,y) t) (eqvo x y o)]
          [(comm i x) (== `((,x comm) . ,y) t) (eqvo x y o)]
          [(assoc i x) (== `((,x assoc) . ,y) t) (eqvo x y o)]
          [(absorp i x) (== `((,x absorp) . ,y) t) (eqvo x y o)]          
          [(distr i x) (== `((,x distr) . ,y) t) (eqvo x y o)]          
          [(dem i x) (== `((,x dem) . ,y) t) (eqvo x y o)]          
          [(dichot i x) (== `((,x dichot) . ,y) t) (eqvo x y o)]          
          ;eight dual equivalence rules - expansion
          [(du-dneg i x) (== `((,x du-sym-dneg) . ,y) t) (eqvo x y o)]
          [(du-idem i x) (== `((,x du-idem) . ,y) t) (eqvo x y o)]
          [(du-comm i x) (== `((,x du-comm) . ,y) t) (eqvo x y o)]
          [(du-assoc i x) (== `((,x du-assoc) . ,y) t) (eqvo x y o)]
          [(du-absorp i x) (== `((,x du-absorp) . ,y) t) (eqvo x y o)]
          [(du-distr i x) (== `((,x du-distr) . ,y) t) (eqvo x y o)]          
          [(du-dem i x) (== `((,x du-dem) . ,y) t) (eqvo x y o)]          
          [(du-dichot i x) (== `((,x du-dichot) . ,y) t) (eqvo x y o)]
          ;Substitution Principle - compound decomposition
          [(mk-not x i) (== `((,x not-comp) . ,res-sub-t) sub-t) 
           (eqvo x res-sub-t sub-o)
           (== `((sub-trace ,sub-t) . ,y) t)
           (== `(~ ,sub-o) new-exp)
           (eqvo new-exp y o)]
          [(mk-and s1 s2 i) 
           (== `((,s1 and-comp-1) . ,res-sub-t-1) sub-t-1) 
           (== `((,s2 and-comp-2) . ,res-sub-t-2) sub-t-2)            
           (eqvo s1 res-sub-t-1 sub-o-1)
           (eqvo s2 res-sub-t-2 sub-o-2)
           (== `((sub-trace ,sub-t-1) (sub-trace ,sub-t-2) . ,y) t)
           (== `(& ,sub-o-1 ,sub-o-2) new-exp)
           (eqvo new-exp y o)]
          [(mk-or s1 s2 i) 
           (== `((,s1 or-comp-1) . ,res-sub-t-1) sub-t-1) 
           (== `((,s2 or-comp-2) . ,res-sub-t-2) sub-t-2)            
           (eqvo s1 res-sub-t-1 sub-o-1)
           (eqvo s2 res-sub-t-2 sub-o-2)
           (== `((sub-trace ,sub-t-1) (sub-trace ,sub-t-2) . ,y) t)
           (== `(// ,sub-o-1 ,sub-o-2) new-exp)
           (eqvo new-exp y o)]
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



