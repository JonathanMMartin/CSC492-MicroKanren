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
(walk u s) -> 
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
  (test-equal? "walk unbound variable"
               (let* ([u (var 0)])
                 (walk u empty-state))
               (var 0)))
