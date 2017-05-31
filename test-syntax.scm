(load "miniKanren-with-symbolic-constraints/mk.scm")

(define (apluso a b c)
  (conde
   [(fresh (rest out)
           (== a `(S ,rest))
           (== c `(S ,out))
           (apluso rest b out))]
   [(== b c)
    (== a 'Z)]
   ))
               
