#lang racket
(provide (all-defined-out))

;; Op0 -> Answer
(define (interp-prim0 op)
  (match op
    ['read-byte (read-byte)]
    ['peek-byte (peek-byte)]
    ['void      (void)]))

;; Op1 Value -> Answer
(define (interp-prim1 op v)
  (match op
    ['add1          (if (integer? v) (add1 v) 'err)]
    ['sub1          (if (integer? v) (sub1 v) 'err)]
    ['zero?         (if (integer? v) (zero? v) 'err)]
    ['char?         (char? v)]
    ['char->integer (if (char? v) (char->integer v) 'err)]
    ['integer->char (if (codepoint? v) (integer->char v) 'err)]
    ['eof-object?   (eof-object? v)]
    ['write-byte    (if (byte? v) (write-byte v) 'err)]
    ['abs (if (integer? v) (abs v) 'err)]
    ['- (if (integer? v) (- v) 'err)]
    ['not (not v)]
    ['integer? (integer? v)]
    ['boolean? (boolean? v)]
    ;; TODO: handle -, abs, integer?, etc.
    [_ 'err]))

;; Op2 Value Value -> Answer
(define (interp-prim2 op v1 v2)
  (match op
    ['< (if (and (integer? v1) (integer? v2)) (< v1 v2) 'err)]
    ['= (if (and (integer? v1) (integer? v2)) (= v1 v2) 'err)]
    ;; TODO: Make + take any number of arguments, see hint below.
    ;; Once that works, you can remove this code:
    ['+ (interp-primN '+ (list v1 v2))]
    ['- (if (and (integer? v1) (integer? v2)) (- v1 v2) 'err)]))

;; HINT: You could use a function like the following and call it from interp.

;; OpN [Listof Value] -> Answer
(define (interp-primN op vs)
  (match op
    ['+ (if (assert-all-int vs) (match vs
          ['() 0]
          [(cons x xs) (+ x (interp-primN '+ xs))]
          ) 'err)]
     ;; TODO: implement n-ary +
    [_ 'err]))

;; Any -> Boolean
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))

(define (assert-all-int xs)
  (match xs
    ['() #t]
    [(cons x xss) (if (integer? x) (assert-all-int xss) #f)]))