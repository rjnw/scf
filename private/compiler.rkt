#lang racket

(module compiler-define-kws racket
  (define-syntax ast (syntax-rules ()))
  (define-syntax language (syntax-rules ()))
  (provide ast language))

(module compiler-stxclass racket
  (require syntax/parse)
  (provide ast-spec language-spec)



  (define-syntax-class ast-def
    #:description "ast definition"
    ;; #:attributes (spec)
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
             #:attr spec (attribute ms.spec))
)

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
    ;; (define (ast-defines)
    ;;   #'())
    ;; (define (language-defines)
    ;;   #'())
    ;;  (ast nodes:ast-spec ...)
    ;;  (language langs:language-spec ...)
    (syntax-parse stx
      [(_ cid:id (ast nodes:ast-spec ...)
          (language langs:language-spec ...))
       (pretty-display (attribute nodes.spec))
       #'0
       ;; (let* ([base-struct-id (format-id #'cid "$~a" #'cid)]
       ;;        [base-struct  #`(struct #,base-struct-id ())])
       ;;   (for/list ([name (syntax->list #'(nodes.name ...))]
       ;;              [vars (syntax->list #'((nodes.var ...) ...))]
       ;;              [defs  (syntax->list #'((nodes.def ...) ...))])
       ;;     (define group-struct-id (format-id #'cid "$~a:~a" #'cid name))
       ;;     (define group-struct #`(struct #,group-struct-id #,base-struct-id ()))
       ;;     (define var-structs
       ;;       (for/list ([var (syntax->list vars)]
       ;;                  [def (syntax->list defs)])
       ;;         (pretty-display (attribute nodes))
       ;;         (define var-struct-id (format-id #'cid "$~a:~a:~a" #'cid name var))

       ;;         #`(struct #,var-struct-id #,group-struct-id #,def)))
       ;;     (pretty-display (syntax->datum group-struct))
       ;;     (pretty-display (map syntax->datum var-structs)))


       ;;   ;; (print (for/list ([name (syntax->datum #'(nodes.name ...))]
       ;;   ;;                   [vars (syntax->datum #'((nodes.var ...) ...))]
       ;;   ;;                   [defs  (syntax->datum #'((nodes.def ...) ...))])
       ;;   ;;          (cons name (for/list ([var vars]
       ;;   ;;                                [def defs])
       ;;   ;;                       (cons var  def)))))
       ;;   ;; (print #'(nodes.name ...))
       ;;   #`(begin 42
       ;;            ;; #,@(ast-defines nodes)
       ;;            ;; #,@(language-defines langs)
       ;;            ))
       ])))

(require 'definer 'compiler-define-kws)
(provide define-compiler)

(module test racket
  (require (submod ".." definer)
           (submod ".." compiler-define-kws))
  ;; (define-compiler c0)
  (define-compiler c1
    (ast
     (terminal
      [n number?:native]
      [str string?:native]
      [sym symbol?:id]
      [t (lam ...)]
      )
     (expression
      [function ('lambda (arg:terminal:sym ...) body:expression)]
      [app (rator:id args:expression ...)]
      [term t:terminal]
      )
     )
    (language
     (l1 (terminals *) (expr app term)))))
