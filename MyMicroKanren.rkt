#lang racket

(module+ test
  ; Import the testing library
  (require rackunit))

#|
Define logic variables using vectors. They should be vectors of a single elemenmt. That element should be an int
|#
(define (var c) (vector c))
(define (var? c) (vector? c))
(define (var=? x y) (= (vector-ref x 0) (vector-ref y 0)))

(define empty-state (cons '() 0))

#|
(walk u s) -> any?
  u: a logic variable
  s: a subsition mapping/state

Takes in a logic variable and a mapping an returns what that logic variable is mapped to
|#
(define (walk u s)
  (if (var? u)
      (let ([w (assf (lambda (v) (var=? u v)) (car s))])
        (if w
            (walk (cdr w) s)
            u))
      u))

#|
(ext-s v x s) -> pair?
  v: a logic variable
  x: a term
  s: a subsition mapping/state

Takes in a logic variable, a term and a state and returns a state which has the new variable binding added to it
|#
(define (ext-s v x s)
  (let* ([lst (car s)]
         [n (cdr s)]
         [b (cons v x)])
    (cons (cons b lst) (+ n 1))))




(module+ test
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
  (test-equal? "walk 0"
               (walk 5 empty-state)
               5)
  (test-equal? "walk 1"
               (let* ([u (var 0)]
                      [s (cons (list (cons (var 0) 64)) 1)])
                 (walk u s))
               64)
  (test-equal? "walk 2"
               (let* ([u (var 0)]
                      [s (cons (list (cons (var 0) (var 1)) (cons (var 1) 12)) 2)])
                 (walk u s))
               12)
  (test-equal? "walk unbound variable"
               (let* ([u (var 0)])
                 (walk u empty-state))
               (var 0))
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
               (cons (list (cons (var 1) 43) (cons (var 0) 97)) 2)))

; From this point forward, we will assume that ext-s works, for use it to make writting test cases a little easier.
