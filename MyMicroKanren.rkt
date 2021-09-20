#lang racket

(provide var
         var?
         var=?
         walk
         ext-s
         unify
         ===
         call/fresh)

#|
Define logic variables using vectors. They should be vectors of a single elemenmt. That element should be an int
|#
(define (var c) (vector c))
(define (var? c) (vector? c))
(define (var=? x y) (= (vector-ref x 0) (vector-ref y 0)))

#|
(walk u s) -> any?
  u: a logic variable
  s: a substitution mapping

Takes in a logic variable and a mapping an returns what that logic variable is mapped to
|#
(define (walk u s)
  (if (var? u)
      (let ([w (assf (lambda (v) (var=? u v)) s)])
        (if w
            (walk (cdr w) s)
            u))
      u))

#|
(ext-s v x s) -> pair?
  v: a logic variable
  x: a term
  s: a substitution mapping

Takes in a logic variable, a term and a state and returns a state which has the new variable binding added to it
|#
(define (ext-s v x s) (cons (cons v x) s))

#|
(unify u v s) -> pair?/bool
  u: a logic variable
  v: a logic variable
  s: a subsitution mapping

Takes in a pair of logic variables and a state, and attempts to unify the logic variables in the state.
If they are already equivlent in the state, then the original state is returned.
If at least one is unbound, then it is bound to the other in a new state, the new state is returned.
If both terms are pairs, then cars and cdrs will attempt to unfiy recursively.
If the terms are unable to be unified then #f is returned.
|#
(define/match (unify u v s)
  [(_ _ #f) #f]
  [(u v s) (let* ([u2 (walk u s)]
                  [v2 (walk v s)])
             (cond
               [(and (var? u2) (var? v2) (var=? u2 v2)) s]
               [(var? u2) (ext-s u2 v2 s)]
               [(var? v2) (ext-s v2 u2 s)]
               [(and (pair? u2) (pair? v2)) (unify (cdr u2) (cdr v2) (unify (car u2) (car v2) s))]
               [(eq? u2 v2) s]
               [else #f]))])

#|
(=== u v) -> 
  u: a term
  v: a term

Takes in two terms, and returns a goal (function) that takes in a state and will succeed if the terms unify in the given state
|#
(define (=== u v)
  (lambda (s/c) (let ([s2 (unify u v (car s/c))])
                (if s2 (cons s2 (cdr s/c)) '()))))

#|
(call/fresh f) ->
  f: a function, whose body is a goal

Used to create a new ("fresh") logic variable, that satisfies the goal of the body of f, if possible.

|#
(define (call/fresh f)
  (lambda (s/c) ((f (var (cdr s/c))) (cons (car s/c) (+ (cdr s/c) 1)))))
            
