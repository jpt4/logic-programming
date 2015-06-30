;;  prelude.scm  jpt4  UTC20150630
;;  basic mk idioms

(define (nullo? i)
  (== '() i))

(define (lengtho ls len o)
  (conde
    [
