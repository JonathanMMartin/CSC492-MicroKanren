#lang racket

(module+ test
  ; Import the testing library
  (require rackunit))

(require "MyMicroKanren.rkt")

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
                      [t 8]
                      [s empty-state])
                 (ext-s u t s))
               (cons (list (cons (var 0) 8)) 1))
  (test-equal? "ext-s 2"
               (let* ([u (var 0)]
                      [v (var 1)]
                      [t 97]
                      [m 43])
                 (ext-s v m (ext-s u t empty-state)))
               (cons (list (cons (var 1) 43) (cons (var 0) 97)) 2))
  (test-equal? "ext-s bind variable to variable"
               (let* ([u (var 0)]
                      [v (var 1)]
                      [s empty-state])
                 (ext-s u v s))
               (cons (list (cons (var 0) (var 1))) 1))
  ; From this point forward, we will assume that ext-s works, to make writing test cases a little easier.
  ; ext-s-lst tests
  (test-equal? "ext-s-lst empty lists"
               (ext-s-lst '() '() empty-state)
               empty-state)
  (test-equal? "ext-s-lst 1 extention"
               (ext-s-lst (list (var 0)) (list 9) empty-state)
               (ext-s (var 0) 9 empty-state))
  (test-equal? "ext-s-lst 2 extentions"
               (ext-s-lst (list (var 0) (var 1)) (list 101 6) empty-state)
               (ext-s (var 1) 6 (ext-s (var 0) 101 empty-state)))
  (test-equal? "ext-s-lst extend variable to bind variable to variabel"
               (ext-s-lst (list (var 0) (var 1) (var 2)) (list 21 (var 0) (var 1)) empty-state)
               (ext-s (var 2) (var 1) (ext-s (var 1) (var 0) (ext-s (var 0) 21 empty-state))))
; From this point forward, we will assume that ext-s-lst works, to make writing test cases a little easier.
  ; walk tests
  (test-equal? "walk 0"
               (walk 5 empty-state)
               5)
  (test-equal? "walk 1"
               (let* ([u (var 0)]
                      [s (ext-s u 64 empty-state)])
                 (walk u s))
               64)
  (test-equal? "walk 2"
               (let* ([u (var 0)]
                      [v (var 1)]
                      [s (ext-s-lst (list u v) (list v 12) empty-state)])
                 (walk u s))
               12)
  (test-equal? "walk unbound variable"
               (let* ([u (var 0)])
                 (walk u empty-state))
               (var 0))
  ; unify tests
  (test-equal? "unify two unbound variables"
               (let* ([u (var 0)]
                      [v (var 1)]
                      [s empty-state])
                 (unify u v s))
               (ext-s (var 0) (var 1) empty-state))
  (test-equal? "unify unbound variable with number"
               (let* ([u (var 0)]
                      [v 1]
                      [s empty-state])
                 (unify u v s))
               (ext-s (var 0) 1 empty-state))
  (test-equal? "unify unbound variable with variable bound to number"
               (let* ([u (var 0)]
                      [v (var 1)]
                      [s (ext-s u 23 empty-state)])
                 (unify u v s))
               (ext-s-lst (list (var 0) (var 1)) (list 23 23) empty-state))
  (test-equal? "attempt to unify two variables bound to different numbers"
               (let* ([u (var 0)]
                      [v (var 1)]
                      [s (ext-s-lst (list u v) (list 55 78) empty-state)])
                 (unify u v s))
               #f)
  (test-equal? "unify two pairs"
               (let* ([u (var 0)]
                      [u1 (var 1)]
                      [u2 (var 2)]
                      [v (var 3)]
                      [v1 (var 4)]
                      [v2 (var 5)]
                      [s (ext-s-lst (list u v) (list (cons u1 u2) (cons v1 v2)) empty-state)])
                 (unify u v s))
               (let* ([u (var 0)]
                      [u1 (var 1)]
                      [u2 (var 2)]
                      [v (var 3)]
                      [v1 (var 4)]
                      [v2 (var 5)])
                 (ext-s-lst (list u v u1 u2) (list (cons u1 u2) (cons v1 v2) v1 v2) empty-state)))
  ; === tests
  (test-equal? "=== failure 0"
               ((=== 0 1) empty-state)
               '())
  (test-equal? "=== failure 1"
               ((=== (var 0) 1) (ext-s (var 0) 200 empty-state))
               '())
  (test-equal? "=== failure 2"
               ((=== (var 0) (var 1)) (ext-s-lst (list (var 0) (var 1)) (list 87 33) empty-state))
               '())
  (test-equal? "=== sucess with x = 1"
               ((=== (var 0) 39) empty-state)
               (ext-s (var 0) 39 empty-state))
  )
                 
