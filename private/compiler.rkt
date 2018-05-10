#lang racket


(module spec-structs racket
  (provide (all-defined-out))

  (struct spec:ast:group (name defs) #:prefab)
  (struct spec:ast:node (var pat) #:prefab)
  (struct spec:ast:pat:single   (spec) #:prefab)
  (struct spec:ast:pat:datum    (spec) #:prefab)
  (struct spec:ast:pat:multiple (spec) #:prefab)
  (struct spec:ast:pat:repeat   (spec) #:prefab))

(module compiler-stxclass racket
  (require syntax/parse)
  (provide ast-spec language-spec)
  (require (submod ".." spec-structs))

  (define-syntax-class ast-def
    #:description "ast definition"
    (pattern single:id
             #:attr spec (spec:ast:pat:single (syntax->datum #'single)))
    (pattern ((~datum quote) datum:id)
             #:attr spec (spec:ast:pat:datum (syntax->datum #'datum)))
    (pattern (multiple:multi-ast-def ...)
            #:attr spec (spec:ast:pat:multiple (attribute multiple.spec)))
    ;; (pattern ((~or (~seq repeat:ast-def (~datum ...)) ms:ast-def) ...)
    ;;          #:attr spec (spec:ast:pat:multiple
    ;;                       ((λ (s r) (if s s (map spec:ast:pat:repeat r)))
    ;;                        (attribute ms.spec)
    ;;                        (attribute repeat.spec))))
    )
 (define-splicing-syntax-class multi-ast-def ;
   #:description "ast definition of a list of attributes"
   (pattern (~seq repeat:ast-def (~datum ...))
            #:attr spec (spec:ast:pat:repeat (attribute repeat.spec)))
   (pattern ms:ast-def
            #:attr spec (attribute ms.spec)))

  (define-syntax-class ast-spec
    #:description "ast specification"
    (pattern (name:id (vars:id defs:ast-def) ...)
             #:attr spec (spec:ast:group #'name (map spec:ast:node (syntax->list #'(vars ...)) (attribute defs.spec)))))

  (define-syntax-class language-spec
    #:description "language specification"
    (pattern (lang:id (name:id var:id ...) ...))))


(module definer racket
  (require (for-syntax (submod ".." compiler-stxclass)
                       (submod ".." spec-structs)
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
          [(spec:ast:pat:single s)  s]
          [(spec:ast:pat:multiple m) (map rec m)]
          [(spec:ast:pat:repeat r) (rec r)]
          [(spec:ast:pat:datum _) '()]))
      (rec node-pat))

    (define (node-pat-format node-pat)
      (define (rec pat args)
        (match* (pat args)
          [((spec:ast:pat:single s) p) #`,#,p]
          [((spec:ast:pat:multiple ms) mps)
           #`(#,@(map rec ms mps))]
          [((spec:ast:pat:repeat r) rp)
           #`,@`#,(rec r rp)]
          [((spec:ast:pat:datum q) _) #`#,q]))
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
        [(spec:ast:group name defs)
         #`(begin
             (struct #,(full-id (cons name lineage)) ())
             ;; (build-group-reader lineage type)
             #,@(map (curry build-defs (cons name lineage)) defs))]
        [(spec:ast:node var pat)
         (build-node-defs var pat lineage)]))

    (define (build-node-defs var pat lineage)
      #`(struct #,(full-id (cons var lineage)) #,(full-id lineage) #,(flatten (node-args pat))
          #:methods gen:custom-write (#,(node-writer var pat lineage))))

    #;(define (build-defs cid full-spec)
        (flatten
         (for/list ([node full-spec])
           (define node-type (car node))
           (define node-id (format-id node-type "~a:~a" cid node-type))
           (list
            #`(struct #,node-id ())
            #`(define (#,(format-id node-type "sexp->~a" node-id) s)
                (match s
                  #,@(for/list ([ast-spec-pair (cdr node)])
                       (define ast-name (car ast-spec-pair))
                       (define ast-spec (cdr ast-spec-pair))
                       (define ast-args (get-ast-args ast-spec))
                       (define ast-arg-types (get-ast-types ast-spec))
                       #`(`#,(ast-format (cdr ast-spec-pair))
                          #,@(if (and (equal? (length ast-args) 1)
                                      (equal? (car ast-arg-types) 'native))
                                 (list #'#:when
                                       #`(#,(get-arg-native-check
                                             (car (get-ast-full-args ast-spec)))
                                          #,(car ast-args)))
                                 '())
                          (#,(format-id ast-name "~a:~a:~a" cid node-type ast-name)
                           #,@ (map
                                (λ (id type)
                                  (if (equal? type 'native)
                                      id
                                      #`(#,(format-id ast-name "sexp->~a:~a"
                                                      cid type) #,id)))
                                ast-args ast-arg-types))))))
            (for/list ([ast-spec-pair (cdr node)])
              (define ast-name (car ast-spec-pair))
              (define ast-spec (cdr ast-spec-pair))
              (define ast-args (get-ast-args ast-spec))
              (define ast-args-syntax
                (map (λ (x) (datum->syntax ast-name x)) ast-args))
              (define attr-id
                (format-id ast-name "~a:~a:~a" cid node-type ast-name))
              (define writer (get-writer attr-id ast-spec ast-args))
              #`(struct #,attr-id #,node-id #,ast-args
                  #:methods gen:custom-write ( #,writer)))))))
    (define (parse-ast stx)
      (syntax-parse stx
        [(ast nodes:ast-spec ...)
         (attribute nodes.spec)])))

  (define-syntax (define-ast stx)
    (syntax-parse stx
      [(_ cid:id ast-stx:expr)
       (define ast-spec (parse-ast #'ast-stx))
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


  (define-ast c1
    (ast
     (terminal
      [num n:native.number?]
      [str str:native.string?]
      [sym sym:native.symbol?])
     (expression
      [function ('lambda (arg:terminal.sym ...) body:expression)]
      [app (rator:expression args:expression ...)]
      [term t:terminal])))

  #;(define-ast sham
      (def
        [function      (arg-ids arg-types ret-type body)]
        [type          (type)]
        [global        (type)]
        [global-string (str)]
        #:common [info #:mutable])
      (type
       [internal ()]
       [ref      (to)]
       [struct   (fields types)]
       [function (args ret)]
       [pointer  (to)]
       [array    (of size)]
       [vector   (of size)])
      (stmt
       [set!     (lhs:expr.var val:expr)]
       [if       (test:expr then:stmt else:stmt)]
       [switch   (test:expr (check:expr body:stmt) ... default)]
       [break    ()]
       [while    (test:expr body:stmt)]
       [return   (value:expr)]
       [void     ()]
       [expr     (e:expr)]
       [block    (stmts:stmt ...)])
      (expr
       [app      (rator:expr rands:expr ...)]
       [void     ()]
       [sizeof   (t:type)]
       [type     (t:type)]
       [gep      (pointer:expr indexes:expr ...)]
       [var      (id:terminal.sym)]
       [global   (id:terminal.sym)]
       [external (lib-id:terminal.sym id:terminal.sym t:type)]
       [let      ((ids:terminal.sym vals:expr types:type)
                  ... stmt:stmt expr:expr)]
       #:sub (const
              [fl     (value:terminal.float        type:type)]
              [si     (value:terminal.signed-int   type:type)]
              [ui     (value:terminal.unsigned-int type:type)]
              [string (value:terminal.string       type:type)]
              [llvm   (value:terminal.llvm         type:type)]
              [struct (value:terminal.struct       type:type)]
              [array  (value:terminal.array        type:type)]
              [vector (value:terminal.vector       type:type)]))
      (rator
       [symbol    (id:terminal.sym)]
       [intrinsic (id:terminal.sym return-type:type)]
       [external  (lib-id:terminal.sym id:terminal.sym ret-type:type)]
       [racket    (id:terminal.sym racket-value:terminal.rkt full-type:type)])))
