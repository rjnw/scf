#lang racket
(require rcf/ast)


;; old
(define-ast LC
  (expr
   [lambda ((n:terminal.sym) body:expr)]
   [letrec (((ids:terminal.sym vals:expr) ...) e:expr)]
   [app (rator:expr rand:expr)]
   [sym s:terminal.sym])
  (terminal #:terminals
            [n number?]
            [sym symbol?]))

;; (define LC2
;;   (extend-ast
;;    LC
;;    (expr
;;     ('let ((ids:terminal.sym vals:expr) ...) e:expr)
;;     ('begin e:expr ...))
;;    (stmt
;;     ('set! id:terminal.sym val:expr))))

;; (define-language L1 #:ast LC2
;;   (expr * (- let))
;;   (terminal n sym))

;; (define-language L2 #:ast LC2 #:extend L1
;;   (+ (stmt *)))

;; (define-pass P1 (L2 -> L1)
;;   [(lambda ())])


;; ($LC:expr `(lambda (n) 123))
;; ($LC:expr `(lambda (12) 123))
;; ($LC:expr `(letrec ([a 1] [b 2]) b))
