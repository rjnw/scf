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

    (define (get-printer spec attrs)
      (define (get-ast-printer spec attrs k)
        (printf "get-ast-printer: ~a ~a\n" spec attrs)
        (match* (spec attrs)
          [(`(single . ,s) p) (k (car p) (cdr p))]
          [(`(multiple . ,ms) mps)
           (get-ast-printer-multiple ms mps k)]
          [(`(repeat . ,r) rp)
           (get-ast-printer r rp (λ (v attrs) (k `(,v '*) attrs)))]
          [(`(quoted . ,q) _) (k q attrs)]))
      (define (get-ast-printer-multiple spec attrs k)
        (printf "get-ast-printer-multiple: ~a ~a\n" spec attrs)
        (if (empty? spec)
            (k '() attrs)
            (get-ast-printer
             (car spec) attrs
             (λ (v attrs)
               (get-ast-printer-multiple (cdr spec) attrs (λ (nv nattrs) (k (cons v nv) nattrs)))))))
      (get-ast-printer spec attrs (λ (v _) v)))

    (get-printer
     `(multiple . ((quoted . lambda) (multiple . ((repeat . (single . arg:terminal.sym))))
                                     (single . body:expression)))
     '(args body))
    )



  (define-syntax (define-compiler stx)
    (define (build-struct-defs cid full-spec)
      #`(begin
          #,@(for/list ([node full-spec])
               (define node-type (car node))
               (define node-id (format-id node-type "~a:~a" cid node-type))
               (define node-format (format-id node-type "<~a:~a>" cid node-type))
               (cons
                #`(struct #,node-id ())
                (for/list ([ast-spec-pair (cdr node)])
                  (define ast-name (car ast-spec-pair))
                  (define ast-spec (cdr ast-spec-pair))
                  (define ast-args (flatten (get-ast-args ast-spec)))
                  (print "printer: \n")
                  (pretty-display (get-printer ast-spec ast-args))
                  (let ([attr-id (format-id ast-name "~a:~a:~a" cid node-type ast-name)])
                    #`(struct #,attr-id #,node-id #,ast-args)))))))
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
       (pretty-display full-spec)
       (pretty-display (syntax->datum full-value))
       (pretty-display (syntax->datum (build-struct-defs #'cid full-spec)))

       #`(define cid #,full-value)])))

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
     (l1 (terminals *) (expr app term)))))
