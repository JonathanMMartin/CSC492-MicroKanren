#lang racket

(module+ test
  ; Import the testing library
  (require rackunit))

(require "MyMicroKanren.rkt")

(define empty-subsitution '())
(define empty-state (cons empty-subsitution 0))

#|
(ext-s-lst vs xs s) -> pair?
  vs: a list of logic variables
  xs: a list of terms
  s: a subsitition mapping

Takes in a list of logic variables, and a list of terms, and a state and returns a state which has new variable bindings with
the first variable mapped to the first term, the second variable mapped to the second term and so on.

Assumes that the two lists are of equal length

This is mostly to make writing test cases easier, use with caution.

|#
(define (ext-s-lst vs xs s)
  (foldl ext-s s vs xs))


(module+ test
  ; Basic var tests
  (test-equal? "var=? 1"
               (let* ([x (var 0)]
                      [y (var 1)])
                 (var=? x y))
               #f)
  (test-equal? "var=? 2"
               (let* ([x (var 0)]
                      [y (var 0)])
                 (var=? x y))
               #t)
  ; ext-s tests
  (test-equal? "ext-s 1"
               (let* ([u (var 0)]
                      [t 8])
                 (ext-s u t empty-subsitution))
               (list (cons (var 0) 8)))
  (test-equal? "ext-s 2"
               (let* ([u (var 0)]
                      [v (var 1)]
                      [t 97]
                      [m 43])
                 (ext-s v m (ext-s u t empty-subsitution)))
               (list (cons (var 1) 43) (cons (var 0) 97)))
  (test-equal? "ext-s bind variable to variable"
               (let* ([u (var 0)]
                      [v (var 1)])
                 (ext-s u v empty-subsitution))
               (list (cons (var 0) (var 1))))
  ; From this point forward, we will assume that ext-s works, to make writing test cases a little easier.
  ; ext-s-lst tests
  (test-equal? "ext-s-lst empty lists"
               (ext-s-lst '() '() empty-subsitution)
               empty-subsitution)
  (test-equal? "ext-s-lst 1 extention"
               (ext-s-lst (list (var 0)) (list 9) empty-subsitution)
               (ext-s (var 0) 9 empty-subsitution))
  (test-equal? "ext-s-lst 2 extentions"
               (ext-s-lst (list (var 0) (var 1)) (list 101 6) empty-subsitution)
               (ext-s (var 1) 6 (ext-s (var 0) 101 empty-subsitution)))
  (test-equal? "ext-s-lst extend variable to bind variable to variabel"
               (ext-s-lst (list (var 0) (var 1) (var 2)) (list 21 (var 0) (var 1)) empty-subsitution)
               (ext-s (var 2) (var 1) (ext-s (var 1) (var 0) (ext-s (var 0) 21 empty-subsitution))))
; From this point forward, we will assume that ext-s-lst works, to make writing test cases a little easier.
  ; walk tests
  (test-equal? "walk 0"
               (walk 5 empty-subsitution)
               5)
  (test-equal? "walk 1"
               (let* ([u (var 0)]
                      [s (ext-s u 64 empty-subsitution)])
                 (walk u s))
               64)
  (test-equal? "walk 2"
               (let* ([u (var 0)]
                      [v (var 1)]
                      [s (ext-s-lst (list u v) (list v 12) empty-subsitution)])
                 (walk u s))
               12)
  (test-equal? "walk unbound variable"
               (let* ([u (var 0)])
                 (walk u empty-subsitution))
               (var 0))
  ; unify tests
  (test-equal? "unify two unbound variables"
               (let* ([u (var 0)]
                      [v (var 1)])
                 (unify u v empty-subsitution))
               (ext-s (var 0) (var 1) empty-subsitution))
  (test-equal? "unify unbound variable with number"
               (let* ([u (var 0)]
                      [v 1])
                 (unify u v empty-subsitution))
               (ext-s (var 0) 1 empty-subsitution))
  (test-equal? "unify unbound variable with variable bound to number"
               (let* ([u (var 0)]
                      [v (var 1)]
                      [s (ext-s u 23 empty-subsitution)])
                 (unify u v s))
               (ext-s-lst (list (var 0) (var 1)) (list 23 23) empty-subsitution))
  (test-equal? "attempt to unify two variables bound to different numbers"
               (let* ([u (var 0)]
                      [v (var 1)]
                      [s (ext-s-lst (list u v) (list 55 78) empty-subsitution)])
                 (unify u v s))
               #f)
  (test-equal? "unify two pairs"
               (let* ([u (var 0)]
                      [u1 (var 1)]
                      [u2 (var 2)]
                      [v (var 3)]
                      [v1 (var 4)]
                      [v2 (var 5)]
                      [s (ext-s-lst (list u v) (list (cons u1 u2) (cons v1 v2)) empty-subsitution)])
                 (unify u v s))
               (let* ([u (var 0)]
                      [u1 (var 1)]
                      [u2 (var 2)]
                      [v (var 3)]
                      [v1 (var 4)]
                      [v2 (var 5)])
                 (ext-s-lst (list u v u1 u2) (list (cons u1 u2) (cons v1 v2) v1 v2) empty-subsitution)))
  ; === tests
  (test-equal? "=== failure 0 =/= 1"
               ((=== 0 1) empty-state)
               '())
  (test-equal? "=== failure x =/= 1, becuase x = 200"
               ((=== (var 0) 1) (cons (ext-s (var 0) 200 empty-subsitution) 1))
               '())
  (test-equal? "=== failure x =/= y, because x = 87, and y = 33"
               ((=== (var 0) (var 1)) (cons (ext-s-lst (list (var 0) (var 1)) (list 87 33) empty-state) 2))
               '())
  (test-equal? "=== success with x = 1"
               ((=== (var 0) 39) (cons empty-subsitution 1)) ;(var 0) is created before the relation is run it just isn't bound to anything hence why the variable count is 1
               (cons (ext-s (var 0) 39 empty-subsitution) 1))
  (test-equal? "=== success with x = y"
               ((=== (var 0) (var 1)) (cons empty-subsitution 2))
               (cons (ext-s (var 0) (var 1) empty-subsitution) 2))
  (test-equal? "=== sucess with (x, y) = (#t, x)"
               ((=== (cons (var 0) (var 1)) (cons #t (var 0))) (cons empty-subsitution 2))
               (cons (ext-s-lst (list (var 0) (var 1)) (list #t #t) empty-subsitution) 2))
  (test-equal? "=== failure with (x, #f) =/= (#t, x)"
               ((=== (cons (var 0) #f) (cons #t (var 0))) (cons empty-subsitution 1))
               '())
  (test-equal? "=== sucess with x = (y, 4)"
               ((=== (var 0) (cons (var 1) 4)) (cons empty-subsitution 2))
               (cons (ext-s (var 0) (cons (var 1) 4) empty-subsitution) 2))
  ; call/freah tests
  (test-equal? "call/fresh 0"
               ((call/fresh (lambda (x) (=== x x))) empty-state)
               (cons empty-subsitution 1))
  (test-equal? "call/fresh 1"
               ((call/fresh (lambda (x) (=== x 1))) empty-state)
               (cons (ext-s (var 0) 1 empty-subsitution) 1))
  )
                 
