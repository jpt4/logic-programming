;; slp.scm
;; UTC20170909
;; jpt4
;; sentential logic prover, from Robert Katz, 1994,
;; "Classical Sentential Logic Based on Logical Equivalence"
;; Chez Scheme v9.4.1

(load "miniKanren-with-symbolic-constraints/mk.scm")
(load "miniKanren-with-symbolic-constraints/numbers.scm")
;;match.ss for dual rule generator
(load "match.ss")

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

;;Characteristic, symmetric, and dual equivalence rule generation

;build-rule 
;given a characteristic rule, produce a characteristic rule procedure
;ex. (build-rule (~ (& a b)) (& (~ a) (~ b)) => 
#;(lambda (i o)
  (conde
   [(fresh (a b)
           (any-statement a) (any-statement b)
           (== `(~ (& ,a ,b)) i)
           (== `(& (~ ,a) (~ ,b)) o))]))

(define-syntax build-rule
  (syntax-rules ()
      [(_ n l r) ;name, lhs, rhs
       (let* ([var-list (filter (lambda (a) 
                                  (not (or (eq? '~ a) (eq? '& a) (eq '// a))))
                                (list-flatten lhs))]
              [lhs-with-vars (replace-sym-with-var lhs)]
              [rhs-with-vars (replace-sym-with-var rhs)]
              [any-statements (map (lambda (a)
                                     (syntax->datum #`(any-statement #,a)))
                                     var-list)]
              [fresh-clause (append `(fresh ,var-list)
                                    any-statements
                                    `((== ,lhs-with-vars i))
                                    `((== ,rhs-with-vars o)))])
         (lambda (i o)
             (conde
              [fresh-clause])))]))

#;(define-syntax build-rule
  (syntax-rules ()
      [(_ n l r)
       (list n l r)]))

(define connectives '(~ & //))                       

(define (replace-sym-with-var exp)
  (cond
   [(null? exp) exp]
   [(member (car exp) connectives) (cons (car exp) 
                                         (replace-sym-with-var (cdr exp)))]
   [(pair? (car exp))
    (cons (replace-sym-with-var (car exp)) (replace-sym-with-var (cdr exp)))]
   [(let ([cexp (car exp)])
      (and (symbol? cexp) (not (member cexp connectives))) 
      (cons (syntax->datum #`,#,cexp) (replace-sym-with-var (cdr exp))))]))    

(define (flat-list? ls)
  (andmap (lambda (a) (not (pair? a))) ls))

(define (list-flatten ls)
  (cond
   [(null? ls) '()]
   [(not (list? ls)) ls]
   [(pair? (car ls)) (append (list-flatten (car ls))
                           (list-flatten (cdr ls)))]
   [else (cons (car ls) (list-flatten (cdr ls)))]))

;rule r does not need to be passed quoted. (sym dneg 'x q) => (~ (~ x))
(define (sym r i o) 
  (r o i))

;Duality

;; Eight Assumed Equivalence Rules, Symmetries, and Duals

;Double Negation
(define (dneg i o)
  (conde
   [(fresh (x)
           (any-statement x)
           (== `(~ (~ ,x)) i)
           (== x o))]))
(define (sym-dneg i o) (sym dneg i o))
;Idempotency
(define (idem i o)
  (conde
   [(fresh (x)
           (any-statement x)
           (== `(& ,x ,x) i)
           (== x o))]))
(define (sym-idem i o) (sym idem i o))
;Commutativity
(define (comm i o)
  (conde
   [(fresh (x y)
           (any-statement x) (any-statement y)
           (== `(& ,x ,y) i)
           (== `(& ,y ,x) o))]))
(define (sym-comm i o) (sym comm i o))
;Associativity
(define (assoc i o)
  (conde
   [(fresh (x y z)
           (any-statement x) (any-statement y) (any-statement z)
           (== `(& ,x (& ,y ,z)) i)
           (== `(& (& ,x ,y) ,z) o))]))
(define (sym-assoc i o) (sym assoc i o))
;Absorption
(define (absorp i o)
  (conde
   [(fresh (x y)
           (any-statement x) (any-statement y)
           (== `(& ,x (// ,x ,y)) i)
           (== x o))]))
(define (sym-absorp i o) (sym absorp i o))
;Distributivity
(define (distr i o)
  (conde
   [(fresh (x y z)
           (any-statement x) (any-statement y) (any-statement z)
           (== `(& ,x (// ,y ,z)) i)
           (== `(// (& ,x ,y) (& ,y ,z)) o))]))
(define (sym-distr i o) (sym distr i o))
;DeMorgan's Rule
(define (dem i o)
  (conde
   [(fresh (x y)
           (any-statement x) (any-statement y)
           (== `(~ (& ,x ,y)) i)
           (== `(& (~ ,x) (~ ,y)) o))]))
(define (sym-dem i o) (sym dem i o))
;Dichotomy
(define (dichot i o)
  (conde
   [(fresh (x y)
           (any-statement x) (any-statement y)
           (== `(// ,x (~ ,x)) i)
           (== `(// ,y (~ ,y)) o))]))
(define (sym-dichot i o) (sym dichot i o))

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
          ;eight symmetric equivalence rules - expansion
          [(sym-dneg i x) (== `((,x sym-sym-dneg) . ,y) t) (eqvo x y o)]
          [(sym-idem i x) (== `((,x sym-idem) . ,y) t) (eqvo x y o)]
          [(sym-comm i x) (== `((,x sym-comm) . ,y) t) (eqvo x y o)]
          [(sym-assoc i x) (== `((,x sym-assoc) . ,y) t) (eqvo x y o)]
          [(sym-absorp i x) (== `((,x sym-absorp) . ,y) t) (eqvo x y o)]
          [(sym-distr i x) (== `((,x sym-distr) . ,y) t) (eqvo x y o)]          
          [(sym-dem i x) (== `((,x sym-dem) . ,y) t) (eqvo x y o)]          
          [(sym-dichot i x) (== `((,x sym-dichot) . ,y) t) (eqvo x y o)]
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
           

(define (thunko i o)
  (== (lambda () i) o))



