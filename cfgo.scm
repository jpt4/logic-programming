
;Loading external files either doesn't work or
;is inconsistent. Ergo, fail hard if not loaded
;manually.
(load "minikanren.scm")
(load "mk.scm")

;A walk is acyclic and consists of at least its
;initial point
(define walk
  (lambda (i ls)
    (cond
      [(assoc i ls) (walk (cdr (assoc i ls)) ls)]
      [else i])))

(define assoc-o
  (lambda (i ls o)
    (conde
      [(fresh (a d)
         (== `((,i . ,a) . ,d) ls)
         (caro ls o))]
      [(fresh (d)
         (cdro ls d)
         (assoc-o i d o))])))
