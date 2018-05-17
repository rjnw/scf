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
             #:attr as (cons 'common #'v))
        (pattern (~seq (~datum #:common-auto) v:expr)
             #:attr as (cons 'common-auto #'v)))
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
    (require syntax/datum)
    (define (full-id lineage)
      (foldl (λ (c d) (format-id c "~a:~a" c d)) (car lineage) (cdr lineage)))
    (define (node-args node-pat)
      (define (rec pat)
        (match pat
          [(ast:pat:single s)  s]
          [(ast:pat:multiple m) (map rec m)]
          [(ast:pat:repeat r) (rec r)]
          [(ast:pat:datum _) '()]))
      (flatten (rec node-pat)))

    (define (meta-args meta-info #:common (cc identity) #:common-auto (ca identity))
      (match meta-info
        ['() '()]
        [`((common . ,c) . ,rst) (cons (cc c) (meta-args rst))]
        [`((common-auto . ,c) . ,rst) (cons (ca c) (meta-args rst))]
        [else (meta-args (cdr meta-info))]))

    (define (node-pat-format var node-pat)
      (printf "node-pat-format: ~a\n" node-pat)
      (define (build-repeat-printer lst)
        (if (list? lst)
            (let ([g-vars (map (λ(v) (gensym 'g)) lst)])
              #`,@(with-datum #,(for/list ([r lst]
                                           [g g-vars])
                                  #`(( #,g #,(datum->syntax #'1 '...)) `#,(build-printer r)))
                    (datum (#,g-vars  #,(datum->syntax #'1 '...)))))
            #`,@`#,(build-printer lst)))
      (define (build-printer v)
        (if (syntax? v)
            v
            (if (ast:pat:single? v)
                #`,#,(ast:pat:single-spec v)
                ;;hack to pass the pattern itself for distinguishing repeat and single
                #`(#,@(map build-printer v)))))
      (define (rec pat)
        (match pat
          [(ast:pat:single s) pat]
          [(ast:pat:multiple m) (map rec m)]
          [(ast:pat:repeat r) (build-repeat-printer (rec r))]
          [(ast:pat:datum d) d]))
      (define p (build-printer (rec node-pat)))
      (if (ast:pat:multiple? node-pat)
          #``(#,var ,@`#,p)
         #``#,p))

    (define (build-group-map spec)
      (match spec
        [`(,groups ...)
         (map build-group-map groups)]
        [(ast:group name _ _ meta-info)
         (cons (syntax->datum name) spec)]))
    (define (build-defs top spec)
      (define group-map (make-hash (build-group-map spec)))
      (define (get-group-spec group-id)
        (if group-id (hash-ref group-map (syntax->datum group-id)) #f))
      (define (group-id spec)
        (match spec
          [#f top]
          [(ast:group name parent _ _)
           (format-id top "~a:~a" (group-id (get-group-spec parent)) name)]))
      (define (group-args group-spec)
        (match-define (ast:group id parent node meta-info) group-spec)
        (append (if parent (group-args (hash-ref group-map (syntax->datum parent))) empty)
                (meta-args meta-info)))
      (define (group-def group-spec)
        (match-define (ast:group name parent node-specs meta-info) group-spec)
        (printf "group-definition: ~a\n" name)
        (define args (meta-args meta-info #:common-auto (λ (v) #`(#,v #:auto))))
        (cons
         (if parent
             #`(struct #,(group-id group-spec) #,(group-id [get-group-spec parent])
                 (#,@args))
             #`(struct #,(group-id group-spec) (#,@args)))
         ;; (group-reader lineage type)
         (map (curry node-def group-spec) node-specs)))
      (define (node-def group-spec node-spec)
        (match-define (ast:node var pat meta-info) node-spec)
        (define node-id (format-id var "~a:~a" (group-id group-spec) var))
        (define writer-args  (append (group-args group-spec) (node-args pat)))
        #`(struct #,node-id #,(group-id group-spec) #,(node-args pat)
            #:methods gen:custom-write
            ((define (write-proc struc port mode)
               (match-define (#,node-id #,@writer-args) struc)
               (display #,(node-pat-format var pat) port)))))
      (flatten (map group-def spec))))

  ;; TODO
  ;; * get parents of super group
  ;; * use the common attributes in writer pattern
  ;; * figure out the reader format with the node names

  (define-syntax (define-ast stx)
    (syntax-parse stx
      [(_ cid:id gs:ast-spec)
       (define ast-spec (attribute gs.spec))
       (define struct-defs (build-defs #'cid ast-spec))
       ;; (printf "struct-defs: ~a\n" struct-defs)
       (pretty-display (map syntax->datum (flatten struct-defs)))
       #`(begin (require syntax/datum) #,@struct-defs)])))

(module test racket
  (require (submod ".." definer))
  (require syntax/datum)
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

  ;; (define-ast sham
  ;;   (ast:expr
  ;;         [let      (((ids:terminal.sym vals:expr types:type)
  ;;                     ...)
  ;;                    stmt:stmt
  ;;                    expr:expr)]
  ;;    ))

  (define-ast sham
    (def
      [function      (arg-ids arg-types ret-type body)]
      [type          (type)]
      [global        (type)]
      [global-string (str)]
      #:common info)
    (ast
     #:common-auto metadata)
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
           [racket    (id:terminal.sym racket-value:terminal.rkt full-type:type)]
           )
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
          [let      (((ids:terminal.sym vals:expr types:type)
                      ...)
                     stmt:stmt
                     expr:expr)])
    (const expr
           [fl     (value:terminal.float        type:type)]
           [si     (value:terminal.signed-int   type:type)]
           [ui     (value:terminal.unsigned-int type:type)]
           [string (value:terminal.string       type:type)]
           [llvm   (value:terminal.llvm         type:type)]
           [struct (value:terminal.struct       type:type)]
           [array  (value:terminal.array        type:type)]
           [vector (value:terminal.vector       type:type)]))
  (pretty-display (sham:ast:expr:let '(a b c) '(1 2 3) '(x y z) 's 'e))
  )
