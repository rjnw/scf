#lang racket


(module ast-syntax-structs racket
  (provide (all-defined-out))

  (struct ast:group (name parent nodes meta-info) #:prefab)
  (struct ast:node (variable pattern meta-info) #:prefab)
  (struct ast:pat:single   (spec) #:prefab)
  (struct ast:pat:datum    (spec) #:prefab)
  (struct ast:pat:multiple (spec) #:prefab)
  (struct ast:pat:repeat   (spec) #:prefab))

(module ast-stxclass racket
  (require syntax/parse)
  (provide ast-spec language-spec)
  (require (submod ".." ast-syntax-structs))

  (define-syntax-class node-pattern
    (pattern single:id
             #:attr spec (ast:pat:single #'single))
    (pattern ((~datum quote) datum:id)
             #:attr spec (ast:pat:datum #'datum))
    (pattern (multiple:node-multiple-pattern ...)
             #:attr spec (ast:pat:multiple (attribute multiple.spec))))
  (define-splicing-syntax-class node-multiple-pattern
    (pattern (~seq repeat:node-pattern (~datum ...))
             #:attr spec (ast:pat:repeat (attribute repeat.spec)))
    (pattern ms:node-pattern
             #:attr spec (attribute ms.spec)))

  (define-splicing-syntax-class node-info
    (pattern (~seq (~datum #:attr) v:expr)
             #:attr as (cons 'attr #'v)))
  (define-syntax-class ast-node
    (pattern (var:id def:node-pattern info:node-info ...)
             #:attr spec (ast:node #'var (attribute def.spec) (attribute info.as))))

  (define-splicing-syntax-class group-info
    (pattern (~seq (~datum #:common) v:expr)
             #:attr as (cons 'common #'v)))
  (define-syntax-class ast-group
    #:description "ast group specification"
    (pattern (name:id (~optional parent:id) nodes:ast-node ... meta:group-info ...)
             #:attr spec (ast:group #'name (attribute parent) (attribute nodes.spec) (attribute meta.as))))
  (define-splicing-syntax-class ast-spec
    (pattern (~seq groups:ast-group ...)
             #:attr spec (attribute groups.spec)))

  (define-syntax-class language-spec
    #:description "language specification"
    (pattern (lang:id (name:id var:id ...) ...))))


(module definer racket
  (require (for-syntax (submod ".." ast-stxclass)
                       (submod ".." ast-syntax-structs)
                       syntax/parse
                       racket/syntax
                       racket/pretty))
  (provide define-ast)

  (begin-for-syntax
    (require racket)
    (define (full-id lineage)
      (foldl (λ (c d) (format-id c "~a:~a" c d)) (car lineage) (cdr lineage)))
    (define (node-args node-pat)
      (define (rec pat)
        (match pat
          [(ast:pat:single s)  s]
          [(ast:pat:multiple m) (map rec m)]
          [(ast:pat:repeat r) (rec r)]
          [(ast:pat:datum _) '()]))
      (rec node-pat))

    (define (node-pat-format node-pat
                             (single (λ (v) #`,#,v))
                             (multiple (λ (vs) #`(#,@vs)))
                             (repeat (λ (v) #`,@`#,v))
                             (datum (λ (v) #`#,v)))
      (define (rec pat args)
        (match* (pat args)
          [((ast:pat:single s) p) (single p)]
          [((ast:pat:multiple ms) mps)
           (multiple (map rec ms mps))]
          [((ast:pat:repeat r) rp)
           (repeat (rec r rp))]
          [((ast:pat:datum q) _) (datum q)]))
      (rec node-pat (node-args node-pat)))

    (define (node-writer var pat lineage)
      #`(define (write-proc struc port mode)
          (match-define (#,(full-id (cons var lineage)) #,@(flatten (node-args pat))) struc)
          (display `#,(node-pat-format pat) port)))

    (define (build-defs lineage type)
      (match type
        [`(,groups ...)
         #`(begin
           #,@(map (curry build-defs lineage) groups))]
        [(ast:group name parent defs meta-info)
         (define nl (cons name (if parent (cons parent lineage) lineage)))
         (printf "meta-info: ~a\n" meta-info)
         (define attrs (map cdr (filter (λ (m) (equal? 'common (car m))) meta-info)))
         (printf "attrs: ~a\n" attrs)
         #`(begin
             (struct #,(full-id nl) (#,@attrs))
             ;; (build-group-reader lineage type)
             #,@(map (curry build-defs nl) defs))]
        [(ast:node var pat meta-info)
         (build-node-defs var pat lineage)]))

    (define (build-node-defs var pat lineage)
      #`(struct #,(full-id (cons var lineage)) #,(full-id lineage) #,(flatten (node-args pat))
          ;; #:methods gen:custom-write (#,(node-writer var pat lineage))
          ))
    )

  (define-syntax (define-ast stx)
    (syntax-parse stx
      [(_ cid:id gs:ast-spec)
       (define ast-spec (attribute gs.spec))
       ;; (pretty-display ast-spec)
       (define struct-defs (build-defs (list #'cid) ast-spec))
       (pretty-display (syntax->datum struct-defs))
       struct-defs])))

(module test racket
  (require (submod ".." definer))
  #;
  (define-compiler LC
    (ast
     (expression
      [function ('lambda (arg:terminal.sym) body:expression)]
      [app (rator:expression rand:expression)]
      [term sym:native.symbol?]))
    (language
     (l1 (expression *))))


  #;(define-ast c1
    (ast
     (terminal
      [num n:native.number?]
      [str str:native.string?]
      [sym sym:native.symbol?])
     (expression
      [function ('lambda (arg:terminal.sym ...) body:expression)]
      [app (rator:expression args:expression ...)]
      [term t:terminal])))

  (define-ast sham
    (def
      [function      (arg-ids arg-types ret-type body)]
      [type          (type)]
      [global        (type)]
      [global-string (str)]
      #:common [info #:mutable])
    (ast
     #:common [metadata #:auto #:mutable])
    (type ast
          [internal ()]
          [ref      (to)]
          [struct   ((fields types) ...)]
          [function (args ret)]
          [pointer  (to)]
          [array    (of size)]
          [vector   (of size)])
    (rator ast
           [symbol    id:terminal.sym]
           [intrinsic (id:terminal.sym return-type:type)]
           [external  (lib-id:terminal.sym id:terminal.sym ret-type:type)]
           [racket    (id:terminal.sym racket-value:terminal.rkt full-type:type)])
    (stmt ast
          [set!     (lhs:expr.var val:expr)]
          [if       (test:expr then:stmt else:stmt)]
          [switch   (test:expr (check:expr body:stmt) ... default)]
          [break    ()]
          [while    (test:expr body:stmt)]
          [return   (value:expr)]
          [void     ()]
          [expr     (e:expr)]
          [block    (stmts:stmt ...)])
    (expr ast
          [app      (rator:expr rands:expr ...)]
          [void     ()]
          [sizeof   (t:type)]
          [etype    (t:type)]
          [gep      (pointer:expr indexes:expr ...)]
          [var      id:terminal.sym]
          [global   (id:terminal.sym)]
          [external (lib-id:terminal.sym id:terminal.sym t:type)]
          [let      ((ids:terminal.sym vals:expr types:type) ...
                                                             stmt:stmt expr:expr)])
    (const expr
           [fl     (value:terminal.float        type:type)]
           [si     (value:terminal.signed-int   type:type)]
           [ui     (value:terminal.unsigned-int type:type)]
           [string (value:terminal.string       type:type)]
           [llvm   (value:terminal.llvm         type:type)]
           [struct (value:terminal.struct       type:type)]
           [array  (value:terminal.array        type:type)]
           [vector (value:terminal.vector       type:type)])))
