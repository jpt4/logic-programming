;;  reverseo.scm
;;  jpt4
;;  revised UTC20150609

(load "miniKanren-with-symbolic-constraints/mk.scm")

(define reverseo
  (lambda (i o)
    (reverse-o-aux i '() o)))

(define reverseo-aux
  (lambda (i acc o)
    (conde
      [(nullo i) (== acc o)]
      [(fresh (a d newacc)
         (conso a d i)
         (conso a acc newacc)
         (reverse-o-aux d newacc o))])))

(define acc-assoco
  (lambda (i l o)
    (conde
      [(nullo l) (== '() o)]
      [(fresh (a d ad res)
         (conso a d l)
         (conso i ad a)
         (conso a res o)
         (acc-assoc-o i d res))]
      [(fresh (a d ad)
         (conso a d l)
         (=/= `(,i . ,ad) a)
         (acc-assoc-o i d o))])))
