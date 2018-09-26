#lang racket
(require "../private/compiler.rkt")




(define-ast sham
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
        [app      (rator:expr rands:expr ...)]
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
            [sym symbol?]
            [float fixnum?]
            [signed-int exact-integer?]
            [unsigned-int exact-nonnegative-integer?]
            [string sham-string?]
            [llvm sham-llvm?]
            [struct sham-struct?]
            [array sham-array?]
            [vector sham-vector?]))

(define (sham-string? s) #f)
(define (sham-llvm? v) #f)
(define (sham-struct? v) #f)
(define (sham-array? v) #f)
(define (sham-vector? v) #f)

(pretty-display (sham:ast:expr:let '(a b c) '(1 2 3) '(x y z) 's 'e))
(pretty-display ($sham:expr `(let ([a 1 (ref i32)] [b 2 (ref i32)] [c 3 (ref i32)]) (svoid) ,(sham:ast:expr:let '(a b c) '(1 2 3) '(x y z) 's 'e))))
;; (module+ test
;;   (require (submod ".." definer))
;;   ;; (require syntax/datum)
;;   #;
;;   (define-compiler LC
;;     (ast
;;      (expression
;;       [function ('lambda (arg:terminal.sym) body:expression)]
;;       [app (rator:expression rand:expression)]
;;       [term sym:native.symbol?]))
;;     (language
;;      (l1 (expression *))))


;;   #;(define-ast c1
;;       (ast
;;        (terminal
;;         [num n:native.number?]
;;         [str str:native.string?]
;;         [sym sym:native.symbol?])
;;        (expression
;;         [function ('lambda (arg:terminal.sym ...) body:expression)]
;;         [app (rator:expression args:expression ...)]
;;         [term t:terminal])))

;;   ;; (define-ast sham
;;   ;;   (ast:expr
;;   ;;         [let      (((ids:terminal.sym vals:expr types:type)
;;   ;;                     ...)
;;   ;;                    stmt:stmt
;;   ;;                    expr:expr)]
;;   ;;    ))


;; )
