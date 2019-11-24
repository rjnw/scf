scf
===
A small DSL for defining Racket's structure types for storing syntax trees.
Example definition of a small lambda calculus:
```
(define-ast LC
  (expr [lambda ((x:expr.sym ...) body:expr)]
        [letrec (((ids:expr.sym vals:expr) ...) body:expr)]
        [app (rator:expr rand:expr ...)]
        [n #:terminal number?]
        [sym #:terminal symbol?]))
```
