#lang racket
(require "../private/compiler.rkt")




(define-ast LC
  (expr
        [lambda ((n:terminal.sym) body:expr)]
        [let (((ids:terminal.sym vals:expr) ...) e:expr)]
        [app (rator:expr rand:expr)]
        [sym s:terminal.sym])
  (terminal #:terminals
            [n number?]
            [sym symbol?]))


($LC:expr `(lambda (n) 123))

($LC:expr `(lambda (12) 123))

($LC:expr `(let ([a 1] [b 2]) b))
