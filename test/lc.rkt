#lang racket
;; (require rcf/ast)
(require "../ast.rkt")

;; old
(define-ast LC
  #:custom-write #t
  #:build-map #t
  #:prefix ||
  #:top-seperator ||
  #:seperator -
  (expr
   [lambda ((x:terminal.sym ...) body:expr)]
   [letrec (((ids:terminal.sym vals:expr) ...) body:expr)]
   [app (rator:expr rand:expr ...)]
   [term v:terminal])
  (terminal #:terminals
            [n number?]
            [sym symbol?]))

(define ex1 (expr-app (expr-term '+) (list (expr-term 2) (expr-term 3))))
(define ex2 (expr-app (expr-term '*) (list (expr-term 2) (expr-app (expr-term '+)
                                                                   (list (expr-term 3)
                                                                         (expr-term 4))))))
(define (plus a b) (expr-app (expr-term '+) (list a b)))
(define (mult a b) (expr-app (expr-term '*) (list a b)))
(define t expr-term)
(define ex3 (expr-lambda '(a) (plus (t 'a) (plus (t 2) (t 3)))))
(define (constant-fold e)
  (match (map-expr constant-fold identity e)
    [(expr-app (expr-term '+) (list (expr-term v1) (expr-term v2)))
     #:when (and (number? v1) (number? v2))
     (expr-term (+ v1 v2))]
    [(expr-app (expr-term '*) (list (expr-term v1) (expr-term v2)))
     #:when (and (number? v1) (number? v2))
     (expr-term (* v1 v2))]
    [e e]))

(printf "before: ~a, after: ~a\n" ex1 (constant-fold ex1))
(printf "before: ~a, after: ~a\n" ex2 (constant-fold ex2))
(printf "before: ~a, after: ~a\n" ex3 (constant-fold ex3))
