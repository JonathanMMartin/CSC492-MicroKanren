#lang racket

(provide var
         var?
         var=?
         walk
         ext-s
         unify
         ===
         call/fresh
         disj
         conj)

#|
Define logic variables using vectors. They should be vectors of a single elemenmt. That element should be an int
|#
(define (var c) (vector c))
(define (var? c) (vector? c))
(define (var=? x y) (= (vector-ref x 0) (vector-ref y 0)))

(define (unit s/c) (cons s/c mzero))
(define mzero '())

#|
(walk u s) -> any?
  u: a logic variable
  s: a substitution mapping

Takes in a logic variable and a mapping an returns what that logic variable is mapped to, if anything
|#
(define (walk u s)
  (let ([pr (and (var? u) (assf (lambda (v) (var=? u v)) s))])
    (if pr (walk (cdr pr) s) u)))

#|
(ext-s v x s) -> substitution mapping?
  v: a logic variable
  x: a term
  s: a substitution mapping

Takes in a logic variable, a term and a subsitution and returns a subsitution which has the new variable binding added to it
|#
(define (ext-s v x s) (cons (cons v x) s))

#|
(unify u v s) -> pair?/bool
  u: a logic variable
  v: a logic variable
  s: a subsitution mapping

Takes in a pair of logic variables and a subsitution, and attempts to unify the logic variables in the state.
If they are already equivlent in the subsitution mapping, then the original subsitution mapping is returned.
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
                                     (and s2 (unify (cdr u2) (cdr v2) s2)))]
      [else (and (eqv? u2 v2) s)])))

#|
(=== u v) -> goal
  u: a term
  v: a term

Takes in two terms, and returns a goal (function) that takes in a state and will succeed if the terms unify in the given state
|#
(define (=== u v)
  (lambda (s/c)
    (let ([s2 (unify u v (car s/c))])
      (if s2 (unit (cons s2 (cdr s/c))) mzero))))

#|
(call/fresh f) -> goal
  f: a function, whose body is a goal

Used to create a new ("fresh") logic variable, that satisfies the goal of the body of f, if possible.

|#
(define (call/fresh f)
  (lambda (s/c)
    ((f (var (cdr s/c))) (cons (car s/c) (+ (cdr s/c) 1)))))

#|
(disj g1 g2) -> goal
  g1: a goal
  g2: a goal

returns a goal that represents g1 or g2
|#
(define (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))


#|
(conj g1 g2) -> goal
  g1: a goal
  g2: a goal

returns a goal that represents g1 and g2
|#
(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))


#|
(mplus st1 st2) -> stream
  st1: a stream
  st2: a stream

appends st2 to st1
|#
(define (mplus st1 st2)
  (cond
    [(null? st1) st2]
    [(procedure? st1) (lambda () (mplus st2 (st1)))]
    [else (cons (car st1) (mplus st2 (cdr st1)))]))

#|
(bind st g) -> stream
  st1: a stream
  g: a goal

returns a stream containing all states in st that satisfy the goal g 
|#
(define (bind st g)
  (cond
    [(null? st) '()]
    [(procedure? st) (lambda () (bind (st) g))]
    [else (mplus (g (car st)) (bind (cdr st) g))]))
            
