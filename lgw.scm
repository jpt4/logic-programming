(load "webminikanren.scm")

(define (lwg-o l w g o)
  (fresh (f1 f2 f3 d1 d2 d3 a1 a2 b1 b2 c1 c2 e1 e2 g1 g2 fp)
    ;;f1 lion final pop
    (pluso l d1 a1)
    (pluso a2 d2 a1)
    (pluso f1 d3 a2)
    ;;f2 wolf final pop
    (pluso d1 b1 w)
    (pluso b1 d2 b2)
    (pluso f2 d3 b2)
    ;;f3 goat final pop
    (pluso c1 d1 g)
    (pluso c2 d2 c1)
    (pluso c2 d3 f3)
    ;;f1+f2+f3 > 0
    (pluso f1 f2 e1)
    (pluso e1 f3 fp)
    (gth-o fp '())
    
    (== `(l= ,f1 w= ,f2 g= ,f3 fp= ,fp) o)
    ))

(define (lth-o a b)
    (fresh (c)
          (pluso a c b)
              (=/= '() c)
                  (nat?-o c)))

(define (gth-o a b)
    (lth-o b a))

(define (nat?-o i)
    (conde
          [(nullo i)]
              [(fresh (a)
                        (== `(1 . ,a) i)
                               (nat?-o a))]
                  [(fresh (a)
                            (== `(0 . ,a) i)
                                   (=/= '() a)
                                          (nat?-o a))]
                      ))

    
    
  
