#lang racket
(require rcf/ast)

(define-ast sham
  #:custom-write #t
  (def
    [module        (defs:def ...)]
    [function      ((arg-ids:terminal.sym arg-types:type) ... ret-type:type body:stmt)]
    [type          (type:type)]
    [global        (type:type)]
    [global-string (str:terminal.string)]
    #:common-mutable info
    #:common id:terminal.sym)
  (ast
   #:common-auto-mutable metadata)
  (type ast
        [internal ()]
        [ref      (to:terminal.sym)]
        [struct   ((fields:terminal.sym types:type) ...)]
        [function (args:type ... '-> ret:type)]
        [pointer  (to:type)]
        [array    (of:type size:terminal.unsigned-int)]
        [vector   (of:type size:terminal.unsigned-nt)])
  (rator ast
         [symbol    id:terminal.sym]
         [intrinsic (id:terminal.sym return-type:type)]
         [external  (lib-id:terminal.sym id:terminal.sym ret-type:type)]
         [racket    (id:terminal.sym racket-value:terminal.rkt full-type:type)])
  (stmt ast
        [set!     (lhs:expr.var val:expr)]
        [if       (test:expr then:stmt else:stmt)]
        [switch   (test:expr (check:expr body:stmt) ... default:expr)]
        [break    ()]
        [while    (test:expr body:stmt)]
        [return   (value:expr)]
        [svoid     ()]
        [expr     (e:expr)]
        [block    (stmts:stmt ...)])
  (expr ast
        [app      (rator:rator rands:expr ...)]
        [evoid     ()]
        [sizeof   (t:type)]
        [etype    (t:type)]
        [gep      (pointer:expr indexes:expr ...)]
        [global   (id:terminal.sym)]
        [external (lib-id:terminal.sym id:terminal.sym t:type)]
        [let      (((ids:terminal.sym vals:expr types:type)
                    ...)
                   stmt:stmt
                   expr:expr)]
        [var      id:terminal.sym])
  (const expr
         [fl     (value:terminal.float        type:type)]
         [si     (value:terminal.signed-int   type:type)]
         [ui     (value:terminal.unsigned-int type:type)]
         [string (value:terminal.string       type:type)]
         [llvm   (value:terminal.llvm         type:type)]
         [struct (value:terminal.struct       type:type)]
         [array  (value:terminal.array        type:type)]
         [vector (value:terminal.vector       type:type)])
  (terminal #:terminals
            ([sym symbol?]
             [float fixnum?]
             [signed-int exact-integer?]
             [unsigned-int exact-nonnegative-integer?]
             [string sham-string?]
             [llvm sham-llvm?]
             [struct sham-struct?]
             [array sham-array?]
             [vector sham-vector?])))

(define (sham-string? s) #f)
(define (sham-llvm? v) #f)
(define (sham-struct? v) #f)
(define (sham-array? v) #f)
(define (sham-vector? v) #f)

(pretty-display (sham:ast:expr:let '(a b c) '(1 2 3) '(x y z) 's 'e))
(pretty-display (sham:ast:stmt:block '(1 2 3 4)))
(pretty-display (sham:ast:stmt:switch 's '(1 2 3) '(a b c) 'd))
;; (pretty-display ($sham:expr `(let ([a 1 (ref i32)] [b 2 (ref i32)] [c 3 (ref i32)]) (svoid) ,(sham:ast:expr:let '(a b c) '(1 2 3) '(x y z) 's 'e))))

;; (define get-signal
;;   (s$:dfunction
;;    (void) 'get-signal
;;    '(cblock index)
;;    (list s$:f32* s$:i32) s$:f32
;;    (s$:ret (load-signal (s$:v 'cblock) (s$:v 'index)))))

;; (define get-signal-simple
;;   ($sham:def
;;    `(function
;;      ,(void) get-signal
;;      (cblock (pointer (ref f32))) (index (ref i32))
;;      (ref f32)
;;      (return (app load-signal cblock index)))))
;; (define (load-signal s i)
;;   (s$:load (s$:gep^ s i)))
