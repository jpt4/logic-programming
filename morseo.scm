;20130913
(load "minikanren.scm")
(load "mk.scm")

;(define morse-o
 ;  (lambda (i o)
  ;  (conde)))



;v. scheme
(define firstn
  (lambda (ls n)
    (cond
      [(zero? n) '()]
      [(cons (car ls)
         (firstn (cdr ls) (sub1 n)))])))

;v2 miniKanren
;new relation allows a single variable to
;represent an arbitrary length morse string
;<to be written>

(define unnest
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(singleton? (car ls))
       (unnest (cons (caar ls) (cdr ls)))]
      [(pair? (car ls)) (unnest (cons (caar ls) (cons (cdar ls) (cdr ls))))]
      [(cons (car ls) (unnest (cdr ls)))])))

(define singleton?
  (lambda (m)
    (and (pair? m) (null? (cdr m)))))

;flatten v2 scheme
(define flatten
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(singleton? (car ls)) (flatten (cons (caar ls) (cdr ls)))]
      [(pair? (car ls))
       (flatten (cons (caar ls) (cons (cdar ls) (cdr ls))))]
      [else (cons (car ls) (flatten (cdr ls)))])))

;new flatten-o
(define flatten-o
  (lambda (i o depth)
    (conde
      [(pairo i)  (fresh (a)
                    (== a (run depth (q) (uw-o i q)))
                    (last-o a o))]
      [(pairo o)  (fresh (a)
                    (== a (run depth (q) (uw-o q o)))
                    (last-o a i))])))

(define last-o
  (lambda (i o)
    (last-o-aux i o '())))

(define last-o-aux
  (lambda (i o acc)
    (conde
      [(nullo i) (fresh (a)
                   (caro acc a)
                   (== a o))]
      [(fresh (a d res newacc)
         (conso a d i)
         (conso a acc newacc)
         (last-o-aux d o newacc))])))     

(define build-number-o
  (lambda (d b)
    (conda
      [(numbero d) (== (build-num d) b)]
      [(bin-number-o b) (== (build-dec-num b) d)])))

(define bin-number-o
  (lambda (n)
    (only-o n '((0) (1)))))

(define build-dec-num
  (lambda (n)
    (cond
      [(null? n) 0]
      [(and (not (eq? (car n) 'rev)) (zero? (car n)) (singleton? n))
       "error: '(0) is ill-formed"]
      [(not (eq? (car n) 'rev)) (let ([m (cons 'rev (reverse n))])
                                (build-dec-num m))]
      [else (let ([p (cdr n)])
              (cond
                [(null? p) 0]
                [(null? p) 0]
                [(zero? (car p)) (build-dec-num (cons 'rev (cdr p)))]
                [else (+ (expt 2 (sub1 (length p))) (build-dec-num (cons 'rev (cdr p))))]))])))
              
(define singleton?
  (lambda (ls)
    (= (length ls) 1)))


#|
;KEEP WORKING HERE - WHY?
;(define build-num-o
 ; (lambda (d b)
  ;  (conde
   ;   [(odd-o d)]
|#

(define zero-o
  (lambda n
    (= '0 n)))  

(define reverse-o
  (lambda (i o)
    (reverse-o-aux i '() o)))

(define reverse-o-aux
  (lambda (i acc o)
    (conde
      [(nullo i) (== acc o)]
      [(fresh (a d newacc)
         (conso a d i)
         (conso a acc newacc)
         (reverse-o-aux d newacc o))])))
          
;working upwrap function
(define uw-o
  (lambda (i o)
    (uw-o-aux i '() o)))

(define uw-o-aux
  (lambda (i acc o)
    (conde
      [(nullo i) (fresh (r)
                   (reverse-o acc o))]
      [(fresh (a d aa ad newacc)
         (conso a d i)
         (=/= `(,aa . ,ad) a)
         (conso a acc newacc)
         (uw-o-aux d newacc o))]
      [(fresh (a d aa ad newi)
         (conso a d i)
         (conso aa '() a)
         (conso aa d newi)
         (uw-o-aux newi acc o))]
      [(fresh (a d aa ad newi1 newi2)
         (conso a d i)
         (conso aa ad a)
         (=/= '() ad)
         (conso ad d newi1)
         (conso aa newi1 newi2)
         (uw-o-aux newi2 acc o))])))
      

(define unpar-o
  (lambda (i o)
    (conde
      [(pairo i)
       (caro i o)]
      [(fresh (a d)
         (=/= `(,a . ,d) i)
         (== i o))])))
      
(define unwrap-o
  (lambda (i o)
    (conde
      [(fresh (a aa ad d res1 res2)
         (== `(,a . ,d) i)
         (== `(,aa . ,ad) a)
         (=/= '() ad)
         (== `(,ad . ,d) res1)
         (== `(,aa . ,res1) res2)
         (unwrap-o res2 o))]
      [(fresh (a aa d res)
         (== `(,a . ,d) i)
         (== `(,aa . ()) a)
         (== `(,aa . ,d) res)
         (unwrap-o res o))]
      [(fresh (a d aa ad res)
         (== `(,a . ,d) i)
         (=/= `(,aa . ,ad) a)
         (conso a res o)
         (unwrap-o d res))])))

(define singleton-o
  (lambda (ls)
    (fresh (a)
      (conso a '() ls))))    
      
      
(define singleton-o
  (lambda (i)
    (fresh (a)
      (conso a '() i))))

(define ncar-o
  (lambda (i n o)
    (conde
      [(== n '()) (== i o)]
      [(fresh (next res a d)
         (sub1-o n next)
         (caro i res)
         (ncar-o res next o))])))

(define odd-o
  (lambda (g)
    (fresh (a)
      (sub1-o a g)
      (even-o a))))

;(run 5 (q) (old-odd-o '(1 0 1))) => (_.0 _.0)
;old-odd-o calls a conde containing relation
;an even number of times, resulting in
;duplicated answers, unlike the odd number of
;conde calls in even-o.

(define old-odd-o
  (lambda (g)
    (conde
      [(== '(1) g) succeed]
      [(fresh (h)
         (sub1-o g h)
         (even-o1 h))])))
(define even-o1
  (lambda (g)
    (conda
      [(== '() g) succeed]
      [(fresh (h)
         (sub1-o g h)
         (old-odd-o h))])))

(define even-o
  (lambda (g)
    (conde
      [(== '() g) succeed]
      [(fresh (h)
         (sub1-o g h)
         (odd-o1 h))])))
(define odd-o1
  (lambda (g)
    (conda
      [(== '(1) g) succeed]
      [(fresh (h)
         (sub1-o g h)
         (even-o h))])))

;input may be only of morse string type
(define only-o
  (lambda (i ls)
    (conde
      [(fresh (a d)
         (appendo d a i)
         (only-o a ls)
         (only-o d ls))]
      [(membero i ls) succeed])))

;Gavin's alternate only-o
(define g-only-o
    (lambda (i ls)
      (conde
        [(nullo i) succeed]
        [(fresh (a d)
           (== `(,a . ,d) i)
           (membero a ls)
           (g-only-o d ls))])))

;WORKS
#|
(run 6 (q) (fresh (a b c d)
              (only-o a '((*) (-))) 
              (== c `(* * * . ,a))
              (morse-o c `(S E . ,b))
              (== `(,a ,b ,c) q)))

(run 1 (q) (fresh (a b c d)
              (only-o a '((*) (-))) 
              (== c `(* * * . ,a))
              (morse-o c `(S S . ,b))
              (== `(,a ,b ,c) q)))
=> ((a= (* * *) b= () c= (* * * * * *) d= (S S)))

(run 2 (q) (fresh (a b c d)
              (only-o a '((*) (-))) 
              (== c `(* * * . ,a))
              (morse-o c d)
              (== `(S S . ,b) d)
              (== `(a=,a b=,b c=,c d=,d) q)))
=>
((a= (* * *) b= () c= (* * * * * *) d= (S S))
  (a= (* * * *) b= (E) c= (* * * * * * *) d= (S S E)))

|#

;current relation is 1-1 between the length of
;morse string and the number of variables
;needed to represent it.
;v1. miniKanren

(define morse-o
  (lambda (i o)
    (fresh (a)
      (conde
        [(nullo i) (== '() o)]
        [(fresh (count source tar rem res)
           (<=o count '(1 0 1))
           (firstn-o i count source)
           (table-o source morse-alist tar)
           (conso tar res o)
           (ncdr-o i count rem)
           (morse-o rem res))]))))

(define firstn-o
  (lambda (i n o)
    (conde
      [(nullo n) (== o '())]
      [(fresh (res a d next)
         (conso a d i)
         (sub1-o n next)
         (conso a res o)
         (firstn-o d next res))])))

(define add1-o
  (lambda (i o)
    (pluso i '(1) o)))

(define sub1-o
  (lambda (i o)
    (minuso i '(1) o)))

(define rfirstn-o
  (lambda (i n o)
    (conde
      [(nullo n) (== i o)]
      [(fresh (a d next)
         (conso a d i)
         (sub1-o n next)
         (rfirstn-o d next o))])))

(define ncdr-o
  (lambda (i n o)
    (conde
      [(nullo n) (== i o)]
      [(fresh (res next)
         (cdro i res)
         (sub1-o n next)
         (ncdr-o res next o))])))
      
(define table-o
  (lambda (i t o)
    (conde
      [(fresh (s)
         (assoc-o i t s)
         (cdro s o))])))

(define assoc-o
  (lambda (x l y)
    (conde
      [(fresh (a b c d)
         (conso a b l)
         (== `(,x . ,d) a)
         (== y a))]
      [(fresh (d)
         (cdro l d)
         (assoc-o x d y))])))



(define old-list
         '([(== '(*) i) (== 'E o)]
          [(== '(-) i) (== 'T o)]
          [(== '(* *) i) (== 'I o)]
          [(== '(* -) i) (== 'A o)]
          [(== '(- *) i) (== 'N o)]
          [(== '(- -) i) (== 'M o)]
          [(== '(* * *) i) (== 'S o)]
          [(== '(* * -) i) (== 'U o)]
          [(== '(* - *) i) (== 'R o)]
          [(== '(* - -) i) (== 'W o)]
          [(== '(- * *) i) (== 'D o)]
          [(== '(- * -) i) (== 'K o)]
          [(== '(- - *) i) (== 'G o)]
          [(== '(- - -) i) (== 'O o)]
          [(== '(* * * *) i) (== 'H o)]
          [(== '(* * * -) i) (== 'V o)]
          [(== '(* * - *) i) (== 'F o)]       
    ;free [(== '(* * - -) i) (== 'E o)]
          [(== '(* - * *) i) (== 'L o)]
    ;free [(== '(* - * -) i) (== 'E o)]
          [(== '(* - - *) i) (== 'P o)]
          [(== '(* - - -) i) (== 'J o)]
          [(== '(- * * *) i) (== 'B o)]
          [(== '(- * * -) i) (== 'X o)]
          [(== '(- * - *) i) (== 'C o)]
          [(== '(- * - -) i) (== 'Y o)]
          [(== '(- - * *) i) (== 'Z o)]
          [(== '(- - * -) i) (== 'Q o)]
    ;free [(== '(- - - *) i) (== 'E o)]
    ;free [(== '(- - - -) i) (== 'E o)]
          [(== '(* * * * *) i) (== '5 o)]      
          [(== '(* * * * -) i) (== '4 o)]      
          [(== '(* * * - -) i) (== '3 o)]      
          [(== '(* * - - -) i) (== '2 o)]      
          [(== '(* - - - -) i) (== '1 o)]      
          [(== '(- * * * *) i) (== '6 o)]      
          [(== '(- - * * *) i) (== '7 o)]      
          [(== '(- - - * *) i) (== '8 o)]      
          [(== '(- - - - *) i) (== '9 o)]      
          [(== '(- - - - -) i) (== '0 o)]))

;attempted scheme pattern matching
#|
(define parse-old-list
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(= (car ls) `[(== ',a i) (== ',b o)])
       (cons (,a ,b) (parse-old-list (cdr ls)))])))
|#

;scheme version using known structure
(define parse-old-list
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(cons (parse-entry (car ls))
         (parse-old-list (cdr ls)))])))

(define parse-entry
  (lambda e
    (cons (cadr (cadaar e)) (cadar (cdadar e)))))

;miniKanren intrinsic pattern matching
(define parse-old-list-o
  (lambda (ls o)
    (conde
      [(nullo ls) (== '() o)]
      [(fresh (a b rem res)
         (conso `[(== ',a i) (== ',b o)] rem ls)
         (conso `(,a . ,b) res o)
         (parse-old-list-o rem res))])))

(define morse-alist
  (car (run* (q) (parse-old-list-o old-list q))))

#|
;=> (((*) (-)))
(run* (q) (fresh (a b c d)
              (== `([(== ',a i) (== ',c o)]
                    [(== ',b i) (== ',d o)]) 
                '([(== '(*) i) (== 'E o)] 
                  [(== '(-) i) (== 'T o)]))
              (== `(,a ,b) q)))

;=> (((== '(*) i) (== '(-) i)))
(run* (q) (fresh (a b c d)
              (== `([,a ,c]
                    [,b ,d]) 
                '([(== '(*) i) (== 'E o)] 
                  [(== '(-) i) (== 'T o)]))
              (== `(,a ,b) q)))

=> ((((== '(*) i) (== 'E o)) ((== '(-) i) (== 'T o))))
(run* (q) (fresh (a b c d)
              (== `(,a ,b) 
                '([(== '(*) i) (== 'E o)] 
                  [(== '(-) i) (== 'T o)]))
              (== `(,a ,b) q)))

=> ((((== '(-) i) (== 'T o)) (123)))
(run* (q) (fresh (a b c d)
              (conso a b 
                '([(== '(*) i) (== 'E o)] 
                  [(== '(-) i) (== 'T o)]
                  [123]
                  ))
              (== b q)))

;does not work
=> ()
(run* (q) (fresh (a b c d)
              (== `(,a ,b) 
                '([(== '(*) i) (== 'E o)] 
                  [(== '(-) i) (== 'T o)]
                  [123]
                  ))
              (== b q)))
|#

#|
;unnest broken in this version

(define unnest-o
  (lambda (i o)
    (conde
      [(nullo i) (== '() o)]
      [(fresh (a d aa ad new)
         (== `((,aa ,ad) ,d) i)
         (nullo ad)
         (== `(,aa ,d) new)
         (unnest-o new o))]
      [(fresh (a d aa da part new)
         (conso a d i)
         (pairo a)
         (caro a aa)
         (cdro a da)
         (== `(,da ,d) part)
         (conso aa part new)
         (unnest-o new o))]
      [(fresh (a d res)
         (conso a d i)
         (conso a res o)
         (unnest-o d res))])))

|#
