#lang racket
;; (require rcf/ast)
(require "../ast.rkt")
(define-ast hakaru
    #:prefix ||
    #:top-seperator ||
    #:seperator -
  #:build-map #t
  (expr
   [mod (main:expr fns:expr ...)]
   [cvar (var val:expr)]
   [fun (name (args:expr ...) ': ret-type:expr body:expr)]
   [lets (((types vars:expr vals:expr) ...) stmt:stmt body:expr)]
   [var (type sym info)
        #:mutable type #:mutable info
        #:extra (#:methods gen:equal+hash
                 ((define (equal-proc v1 v2 _)
                    (equal? (expr-var-sym v1) (expr-var-sym v2)))
                  (define (hash-proc v _) (equal-hash-code (expr-var-sym v)))
                  (define (hash2-proc v _) (equal-secondary-hash-code (expr-var-sym v)))))]
   [arr (type index:expr size:expr body:expr)]
   [sum (type index:expr start:expr end:expr body:expr)]
   [prd (type index:expr start:expr end:expr body:expr)]
   [bucket (type start:expr end:expr reducer:reducer)]
   [branch (pat:pat body:expr)]
   [match (type tst:expr branches:expr ...)]
   [bind (var body:expr)]
   [if (type tst:expr thn:expr els:expr)]
   [app (type rator:expr rands:expr ...)]
   [val (type v:expr)]
   [intrf (sym)]
   )

  (reducer
   [split (e:expr a:reducer b:reducer)]
   [fanout (a:reducer b:reducer)]
   [add (e:expr)]
   [nop ()]
   [index (n:expr i:expr a:reducer)])

  (stmt
   [return (val)]
   [if (tst:expr thn:stmt els:stmt)]
   [for (i:expr start:expr end:expr body:stmt)]
   [expr (stmt:stmt expr:expr)]
   [block (stmts:stmt ...)]
   [assign (var:expr val:expr)]
   [void ()])

  (pat
   [true ()]
   [false ()]
   [pair (a:pat b:pat)]
   [var ()]
   [ident ()]))
