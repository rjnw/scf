#lang racket


(module compiler-stxclass racket
  (require syntax/parse)
  (provide ast-spec language-spec)

  (define (parse-single-def s)
    (define s-str (symbol->string s))
    (pretty-print s-str)
    (match-define (list id type) (string-split s-str ":"))
    (cons (string-split type ".") id))
  (define-syntax-class ast-def
    #:description "ast definition"
    (pattern single:id
             #:attr spec `(single . ,(syntax->datum #'single)))
    (pattern ((~datum quote) datum:id)
             #:attr spec `(quoted . ,(syntax->datum #'datum)))
    (pattern (multiple:multi-ast-def ...)
             #:attr spec `(multiple . ,(attribute multiple.spec))))

  (define-splicing-syntax-class multi-ast-def
    #:description "ast definition of a list of attributes"
    (pattern (~seq repeat:ast-def (~datum ...))
             #:attr spec `(repeat . ,(attribute repeat.spec)))
    (pattern ms:ast-def
             #:attr spec (attribute ms.spec)))

  (define-syntax-class ast-spec
    #:description "ast specification"
    (pattern (name:id (var:id def:ast-def) ...)
             #:attr spec (attribute def.spec)))

  (define-syntax-class language-spec
    #:description "language specification"
    (pattern (lang:id (name:id var:id ...) ...))))


(module definer racket
  (require (for-syntax (submod ".." compiler-stxclass)
                       racket/list
                       racket/match
                       racket/string
                       syntax/parse
                       racket/syntax
                       racket/pretty))
  (provide define-compiler)
  (begin-for-syntax
    (define (get-id i)
      (define parts (string-split (symbol->string i) ":"))
      (string->symbol (car parts)))
    (define (get-ast-args ast-spec)
      (match ast-spec
        [`(single . ,s) (get-id s)]
        [`(multiple . ,m) (map get-ast-args m)]
        [`(repeat . ,r) (get-ast-args r)]
        [`(quoted . ,_) '()]))

    (define (get-writer name spec attrs)
      (define (get-ast-writer spec attrs k)
        ;; (printf "get-ast-writer: ~a ~a\n" spec attrs)
        (match* (spec attrs)
          [(`(single . ,s) p) (k #`,#,(car p) (cdr p))]
          [(`(multiple . ,ms) mps)
           (get-ast-writer-multiple ms mps k)]
          [(`(repeat . ,r) rp)
           (get-ast-writer r rp (λ (v attrs) (k #`,@`#,v attrs)))]
          [(`(quoted . ,q) _) (k q attrs)]))
      (define (get-ast-writer-multiple spec attrs k)
        ;; (printf "get-ast-writer-multiple: ~a ~a\n" spec attrs)
        (if (empty? spec)
            (k '() attrs)
            (get-ast-writer
             (car spec) attrs
             (λ (v attrs)
               (get-ast-writer-multiple (cdr spec) attrs (λ (nv nattrs) (k (cons v nv) nattrs)))))))
      #`(define (write-proc struc port mode)
          (match-define (#,name #,@attrs) struc)
          (display `#,(get-ast-writer spec attrs (λ (v _) v)) port))))



  (define-syntax (define-compiler stx)
    (define (build-struct-defs cid full-spec)
      (flatten (for/list ([node full-spec])
         (define node-type (car node))
         (define node-id (format-id node-type "~a:~a" cid node-type))
         (define node-format (format-id node-type "<~a:~a>" cid node-type))
         (cons
          #`(struct #,node-id ())
          (for/list ([ast-spec-pair (cdr node)])
            (define ast-name (car ast-spec-pair))
            (define ast-spec (cdr ast-spec-pair))
            (define ast-args (flatten (get-ast-args ast-spec)))
            (define ast-args-syntax (map (λ (x) ( datum->syntax ast-name x)) ast-args))
            (let ([attr-id (format-id ast-name "~a:~a:~a" cid node-type ast-name)])
              (define writer (get-writer attr-id ast-spec ast-args))
              #`(struct #,attr-id #,node-id #,ast-args
                  #:methods gen:custom-write
                  ( #,writer))))))))
    (syntax-parse stx
      [(_ cid:id (ast nodes:ast-spec ...)
          (language langs:language-spec ...))
       (define full-spec
         (for/list ([node-type (syntax->list #'(nodes.name ...))]
                    [type-vars (syntax->list #'((nodes.var ...) ...))]
                    [type-attributes (attribute nodes.spec)])
           (cons node-type
                 (for/list ([type-var (syntax->list type-vars)]
                            [type-attribute type-attributes])
                   (cons type-var type-attribute)))))
       (define full-value
         #`(list #,@(for/list ([node-type (syntax->list #'(nodes.name ...))]
                               [type-vars (syntax->list #'((nodes.var ...) ...))]
                               [type-attributes (attribute nodes.spec)])
                      #`(cons (quote-syntax #,node-type)
                              (list #,@(for/list ([type-var (syntax->list type-vars)]
                                                  [type-attribute type-attributes])
                                         #`(cons (quote-syntax #,type-var)
                                                 (quote #,type-attribute))))))))
       (define struct-defs (build-struct-defs #'cid full-spec))
       (pretty-display full-spec)
       (define defs
         #`(begin
             (define cid #,full-value)
             #,@struct-defs))
       (pretty-display (syntax->datum defs))
       defs])))

(module test racket
  (require (submod ".." definer))
  (define-compiler LC
    (ast
     (expression
      [function ('lambda (arg:terminal.sym) body:expression)]
      [app (rator:expression rand:expression)]
      [term sym:native.symbol?]))
    (language
     (l1 (expression *))))


  (define-compiler c1
    (ast
     (terminal
      [num n:native-check.number?]
      [str str:native.string?]
      [sym sym:native.symbol?])
     (expression
      [function ('lambda (arg:terminal.sym ...) body:expression)]
      [app (rator:id args:expression ...)]
      [term t:terminal]))
    (language
     (l1 (terminals *) (expr app term))))

  (define-ast sham
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
     [let      ((ids:terminal.sym vals:expr types:type) ... stmt:stmt expr:expr)]
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
