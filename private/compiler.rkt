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
    #:description "ast definition"
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
                       syntax/parse
                       racket/syntax
                       racket/pretty))
  (provide define-compiler)
  (define-syntax (define-compiler stx)
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
       #`(define cid (list #,@(for/list ([node-type (syntax->list #'(nodes.name ...))]
                                         [type-vars (syntax->list #'((nodes.var ...) ...))]
                                         [type-attributes (attribute nodes.spec)])
                                #`(cons (quote-syntax #,node-type)
                                        (list #,@(for/list ([type-var (syntax->list type-vars)]
                                                            [type-attribute type-attributes])
                                                   #`(cons (quote-syntax #,type-var)
                                                           (quote #,type-attribute))))))))])))

(module test racket
  (require (submod ".." definer))
  (define-compiler LC
    (ast
     (expression
      [function ('lambda (arg:terminal.sym) body:expression)]
      [app (rator:expression rand:expression)]
      [term symbol?:native-check]))
    (language
     (l1 (expression *))))
  (define-compiler c1
    (ast
     (terminal
      [n number?:native-check]
      [str string?:native-check]
      [sym symbol?:id])
     (expression
      [function ('lambda (arg:terminal.sym ...) body:expression)]
      [app (rator:id args:expression ...)]
      [term t:terminal]))
    (language
     (l1 (terminals *) (expr app term)))))
