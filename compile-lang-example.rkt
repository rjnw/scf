#lang racket

(require "compile-lang.rkt")

(define-grammar math
  (terminals
   (variable  (x))  
   (number    (n)))
  (expr (e) 
   [var : ,x]
   [int : ,n]
   [+   : (+ ,e1 ,e2)]
   [let : (let ([,x* ,e*] ...) ,e)]
   [if  : (if ,e-c ,e-t ,e-a)])
  (folds (f)
         [fold-n : (fold ,x (,e ,e) ,e)])
  (EandF (ef)
   [fold : ,f]
   [expr : ,e]
   [begin : (,ef ...)]))


(define-language math L1
  #:top expr
  (#:+ expr (var int +)))

(define-language math L2
  #:extends L1
  #:top expr
  (#:+ expr (let)))

;; Have to exist already
;; variable?
;; number?

(struct math ())
(struct math:expr math ())
(struct math:expr:var expr (x))
(struct math:expr:int expr (n))
(struct math:expr:+   expr (e1 e2)
  #:methods gen:math:expr
  ((define (map-expr f m:e)
     (match-define (math:expr:+ e1 e2) m:e)
     (math:expr (f e1) (f e2)))
   (define (map-expr/state f m:e s)
     (match-define (math:expr:+ e1 e2) m:e)))
  #:methods gen:math
  ((define (map-math f-expr f-fold f-fe m)
     (match-define (math:expr:+ e1 e2) m:e)
     (math:expr (f-expr e1) (f-expr e2)))))

(define (map-expr-let f e)
  (match e
    [(let ([,x ,e1]) ,e2) (f x e1 e2)]
    [other (map-expr (lambda (e) (map-expr-let f e)) e)]))

(define-pass p1 math l2 -> l1
  [expr
   [(let ([,x ,e1]) ,e2) (replace x e1 e2)]
   [(+ (int ,n1) (int ,n2)) ($int (+ n1 n2))]])
=>
(math$ e)
(let ()
  (local-require math-syntax)
  e)

(define (p1 m)
  (local-require math-syntax)
  (define (p1-expr e)
    (match e
      [(math:expr:let `(,x) `(,e1) e2) (replace x e1 e2)]
      [(math:expr:+ (math:expr:int n1) (math:expr:int n2))
       ($int (+ n1 n2))]
      [other (map-expr p1-expr e)]))
  (p1-expr m))



(struct math:expr:let expr (x* e* e))
(struct math:expr:if  expr (c t a))
(struct math:fold math ())
;; ...

(define-contract L1
  (and math?
       math:expr?
       (L1:expr/c (rec L1:expr/c))))

(define-contract (L1:expr/c expr/c)
  (U (math:expr:var/c variable?)
     (math:expr:int/c number?)
     (math:expr:+/c expr/c expr/c)))

(define-contract L2
  (and math?
       math:expr?
       (L2:expr/c (rec L2:expr/c))))

(define-contract (L2:expr/c expr/c) 
  (U (L1:expr/c expr/c) 
     (math:expr:let/c (list/c variable?)
                      (list/c expr/c)
                      expr/c)))


;; (let$ x* e* e)

;; ($m ($e (let ([x 1] [y 2]) (+ x y))))
;; (math$ (let (x y) (1 2) (+ x y)))

;; (let*$ ([x 1] [y 2]) (+ x y))
;; (expr:let `(g1) `(1)
;;  (expr:let `(g2) `(2)
;;   (expr:+ (expr:var g1) (expr:var g2))))








