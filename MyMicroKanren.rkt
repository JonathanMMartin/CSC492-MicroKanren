#lang racket

(provide var
         var?
         var=?
         empty-state
         walk
         ext-s
         ext-s-lst
         unify)


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

#|
(ext-s-lst vs xs s) -> pair?
  vs: a list of logic variables
  xs: a list of terms
  s: a subsitition mapping/state

Takes in a list of logic variables, and a list of terms, and a state and returns a state which has new variable bindings with
the first variable mapped to the first term, the second variable mapped to the second term and so on.

Assumes that the two lists are of equal length

This is mostly to make writing test cases easier, use with caution.

|#
(define/match (ext-s-lst vs xs s)
  [('() '() s) s]
  [((cons v vs) (cons x xs) s) (ext-s-lst vs xs (ext-s v x s))])

#|
(unify u v s) -> pair?/bool
  u: a logic variable
  v: a logic variable
  s: a subsitution mapping/state

Takes in a pair of logic variables and a state, and attempts to unify the logic variables in the state.
If they are already equivlent in the state, then the original state is returned.
If at least one is unbound, then it is bound to the other in a new state, the new state is returned.
If both terms are pairs, then cars and cdrs will attempt to unfiy recursively.
If the terms are unable to be unified then #f is returned.
|#
(define (unify u v s)
  (let* ([u2 (walk u s)]
         [v2 (walk v s)])
    (cond
      [(and (var? u2) (var? v2) (var=? u2 v2)) s]
      [(var? u2) (ext-s u2 v2 s)]
      [(var? v2) (ext-s v2 u2 s)]
      [(and (pair? u2) (pair? v2)) (let ([s2 (unify (car u2) (car v2) s)])
                                     (if s2
                                         (unify (cdr u2) (cdr v2) s2)
                                         #f))]
      [(eq? u2 v2) s]
      [else #f])))
            
