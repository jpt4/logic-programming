(define q0
  (lambda (i o)
    (fresh (c d)
      (caro i c)
      (cdro i d)
      (conde
        [(== '() i) (== 'even o)]
        [(=/= '() c) (q1 d e)]
        ))))

(define q1
  (lambda (i o)
    (fresh (c d e)
      (caro i c)
      (cdro i d)
      (conde
        [(== '() i) (== 'odd o)]
        [(=/= '() c) (q0 d e)]
        ))))
          
