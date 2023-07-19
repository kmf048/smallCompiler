#lang racket
(provide interp interp-env)
(require "ast.rkt" "interp-prim.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void

;; type Env = (Listof (List Id Value))

;; Expr -> Answer
(define (interp e)
  (interp-env e '()))

;; Expr Env -> Answer
(define (interp-env e r)
  (match e
    [(Int i) i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof) eof]
    [(Var x) (lookup r x)]
    [(Prim0 p) (interp-prim0 p)]
    [(Prim1 p e)
     (match (interp-env e r)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v1 (match (interp-env e2 r)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    ;; TODO: implement n-ary primitive
    [(PrimN p es)
     (match (interp*-env es r)
       ['err 'err]
       [vs (interp-primN p vs)])]
    [(If p e1 e2)
     (match (interp-env p r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(Begin e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v    (interp-env e2 r)])]
    ;; TODO: implement cond
    [(Cond cs e) (interp-cond cs e r)]
    ;; TODO: implement case
    [(Case ev cs el) (interp-case (interp-env ev r) cs el r)]
    [(Let xs es expr) (interp-lets xs (interp*-env es r) expr r)]
    ;; TODO: implement let*
    [(Let* xs es e) (interp-let* xs es e r)]
))

(define (interp-lets xs es expr r)
  (match es
    ['err 'err]
    ['() (interp-env expr r)]
    [(cons v '()) (match xs
                    [(list x) (interp-env expr (cons (list x v) r))])]
    [(cons v vs) (match xs
                   [(cons x ls) (interp-lets ls vs expr (ext r x v))])]))

(define (interp-let* xs es e r)
  (match xs
    ['() (interp-env e r)]
    [(cons x xs)
     (match (interp-env (car es) r)
       ['err 'err]
       [v (interp-let* xs (cdr es) e (ext r x v))])]))


;; HINT: this is a function that may come in handy.
;; It takes a list of expressions and environment
;; and evaluates each expression in order.  If any
;; expression produces 'err, the whole thing produces
;; 'err; otherwise it produces a list of values.

;; type Answer* = 'err | [Listof Value]
;; [Listof Expr] Env -> Answer*
(define (interp*-env es r)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r)
       ['err 'err]
       [v (match (interp*-env es r)
            ['err 'err]
            [vs (cons v vs)])])]))

;; Env Id -> Value
(define (lookup r x)
  (match r
    [(cons (list y val) r)
     (if (symbol=? x y)
         val
         (lookup r x))]))


;; Env Id Value -> Env
(define (ext r x v)
  (cons (list x v) r))

;; UPDATE FOR 'err
(define (interp-cond cs e r)
  (match cs
    ['() (interp-env e r)]
    [(cons (Clause e1 e2) cs)
     (if (interp-env e1 r)
         (interp-env e2 r)
         (interp-cond cs e r))]))
;; UPDATE FOR 'err
(define (interp-case v cs e r)
  (match cs
    ['() (interp-env e r)]
    [(cons (Clause d1 e2) cs)
     (if (memv v d1)
         (interp-env e2 r)
         (interp-case v cs e r))]))

