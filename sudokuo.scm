;UTC20170408
;jpt4
;mk sudoku solver
;Chez Scheme v9.4

(load "miniKanren-with-symbolic-constraints/mk.scm")
(load "miniKanren-with-symbolic-constraints/numbers.scm")
(load "prelude.scm")

(define (vectoro vec len)
  (list?o vec) (lengtho vec len))


   
