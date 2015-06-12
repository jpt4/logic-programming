;20140113 - jpt4
;walko.scm
;walk association lists in miniKanren

;(assoc-o _ '() _) => #f
(define assoc-o
  (lambda (i ls o)
    (conde
      [(nullo ls) (== '#f o)]
      [(fresh (a b)
         (== `((,i . ,a) . ,b) ls)
         (caro ls o))]
      [(fresh (a b c)
         (== `((,a . ,b) . ,c) ls)
         (=/= a i)
         (assoc-o i c o))])))
;----------------------------
;C311 Interlude 20140128
(define c-unify-o
  (lambda (u v l o)
    (conde
      [(== u v) (== u (+)) (== v (+))])))

;von C311
(define ext-S-o
  (lambda (x v S o)
    (conde
      []
     
;----------------------------
(define walk-o
  (lambda (i ls o)
    (conde
      [(fresh (a b c d)
         (assoc-o i ls a)
         (cdro a b)
         (walk-o b ls o))]
      [(fresh (a b c d)
         (assoc-o i ls #f)
         (== o i))])))

(define walk*-o
  (lambda (i ls o)
    (conde
      [(assoc-o i ls #f) (== o i)]
      [(fresh (a b c d e f g)
         (assoc-o i ls a)
         (cdro a b)
         (== `(,c . ,d) b)
         (caro d g)
         (walk*-o c ls e) (walk*-o g ls f)
         (conso `(,e) `(,f) o))]
      [(fresh (a b c d)
         (assoc-o i ls a)
         (cdro a b)
         (=/= `(,c . ,d) b)
         (atom-o b)
         (walk*-o b ls o))])))

(define atom-o
  (lambda (i)
    (conde
      [(symbolo i)]
      [(numbero i)])))

(define atoml-o
  (lambda (ls)
    (conde
      [(fresh (a)
         (== `(,a . ()) ls)
         (atom-o a))]
      [(atom-o ls)]
      [(fresh (a b c d)
;         (appendo a b ls)
;         (conso a b ls)
         (== `(,a . ,b) ls)
         (atom-o a)
         (atoml-o b))])))
#;(define alist?-o
  (lambda (a)
    (conde
      [])))
