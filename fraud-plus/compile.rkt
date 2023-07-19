#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg

;; type CEnv = (Listof ID)

;; Expr -> Asm
(define (compile e)
  (prog (Extern 'peek_byte)
        (Extern 'read_byte)
        (Extern 'write_byte)
        (Extern 'raise_error)
        (Global 'entry)
        (Label 'entry)
        (compile-e e '())
        (Ret)
        (Label 'raise_error_align)
        pad-stack
        (Call 'raise_error)))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Var x)            (compile-variable x c)]
    [(Prim0 p)          (compile-prim0 p c)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    ;; TODO: implement n-ary primitives
    [(PrimN p es)    (compile-primN p es c)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)      (compile-begin e1 e2 c)]
    ;; TODO: this only works for single variable binding,
    ;; make it work in general
    [(Let (list x) (list e1) e2)
     (compile-let1 x e1 e2 c)]
    [(Let xs es e) (compile-let xs es e c)]
    ;; TODO: implement let*, case, cond
    [(Let* xs es e)  (compile-let* xs es e c)]
    [(Case ev cs el) (compile-case ev cs el c)]
    [(Cond cs el)    (compile-cond cs el c)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (compile-op0 p))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (compile-op1 p)))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (compile-op2 p)))

(define (compile-primN p es c)
  (match es
    ['() (Mov rax 0)]
    [(cons x '()) (compile-e x c)]
    [(cons e es) (seq
                  (compile-e e c)
                  (assert-integer rax)
                  (Push rax)
                  (compile-primN p es (cons #f c))
                  (Pop r8)
                  (assert-integer r8)
                  (assert-integer rax)
                  (Add rax r8)
                  )]
      ))

;; HINT: Another potentially helpful function that
;; emits code to execute each expression and push
;; all the values on to the stack, analogous to interp*-env

;; [Listof Expr] CEnv -> Asm
(define (compile-e* es c)
  (match es
    ['() (seq)]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-e* es (cons #f c)))]))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2))))

;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)))

(define (compile-cond cs el c)
    (match cs
      ['() (compile-e el c)]
      [(cons (Clause e1 e2) cs)
       (compile-if e1 e2 (Cond cs el) c)
       ]
      ))

(define (compile-case ev cs el c)
  (let ((end (gensym 'caseend))
        (ls (map (λ (_) (gensym 'rhs)) cs)))
  (seq (compile-e ev c)
       (compile-datums-comparisons cs ls)
       (compile-e el c)
       (Jmp end)
       (compile-rhss cs end ls c)
       (Label end))))

(define (compile-rhss cs end ls c)
  (match (cons cs ls)
    [(cons '() '()) (seq)]
    [(cons (cons (Clause _ e) cs) (cons l ls))
     (seq (Label l)
          (compile-e e c)
          (Jmp end)
          (compile-rhss cs end ls c))]))

(define (compile-datums-comparisons cs ls)
  (match (cons cs ls)
    [(cons '() '()) (seq)]
    [(cons (cons (Clause ds _) cs) (cons l ls))
     (seq (compile-datums-single ds l)
          (compile-datums-comparisons cs ls))]))

(define (compile-datums-single ds l)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (Cmp 'rax (value->bits d))
          (Je l)
          (compile-datums-single ds l))]))

;; Id Expr Expr CEnv -> Asm
;; NOTE: this is specialized for a single variable binding
;; You should write another function for the general case
(define (compile-let1 x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

                   
(define (compile-let xs es e c)
  (let* ((len (length xs))
         (cenv (reverse xs)))
    (seq (compile-e* es c)
         (compile-e e (append cenv c))
         (Add rsp (* len 8)))))

(define (compile-let* xs es e c)
  (match xs
    ['() (compile-e e c)]
    [(cons x xs) (match es
                   ['() 'err]
                   [(cons e1 es) (seq (compile-e e1 c)
                                     (Push rax)
                                     (compile-let* xs es e (cons x c))
                                     (Add rsp 8)
                                     )])]))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))
