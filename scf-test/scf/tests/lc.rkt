#lang racket
(require scf)
;; old
(define-ast LC
  #:custom-write #t
  #:build-map #t
  #:prefix ||
  #:top-seperator ||
  #:seperator -
  (expr
   [lambda ((x:expr.sym ...) body:expr)]
   [letrec (((ids:expr.sym vals:expr) ...) body:expr)]
   [app (rator:expr rand:expr ...)]
   [n #:terminal number?]
   [sym #:terminal symbol?]))

(define ex1 (expr-app (expr-sym '+) (list (expr-n 2) (expr-n 3))))
(define ex2 (expr-app (expr-sym '*) (list (expr-n 2) (expr-app (expr-sym '+)
                                                                   (list (expr-n 3)
                                                                         (expr-n 4))))))
(define (plus a b) (expr-app (expr-sym '+) (list a b)))
(define (mult a b) (expr-app (expr-sym '*) (list a b)))
(define n expr-n)
(define ex3 (expr-lambda (list (expr-sym 'a)) (plus (expr-sym 'a) (plus (n 2) (n 3)))))

(define (constant-fold e)
  (match (map-expr constant-fold e)
    [(expr-app (expr-sym '+) (list (expr-n v) ...))
     (expr-n (apply + v))]
    [(expr-app (expr-sym '*) (list (expr-n v) ...))
     (expr-n (apply * v))]
    [e e]))

(printf "before: ~a, after: ~a\n" ex1 (constant-fold ex1))
(printf "before: ~a, after: ~a\n" ex2 (constant-fold ex2))
(printf "before: ~a, after: ~a\n" ex3 (constant-fold ex3))
