#lang racket

(module compiler-define-kws racket
  (define-syntax ast (syntax-rules ()))
  (define-syntax language (syntax-rules ()))
  (provide ast language))

(module compiler-stxclass racket
  (require syntax/parse)
  (provide ast-spec language-spec)

  (define-syntax-class ast-spec
    #:description "ast specification"
    (pattern (name:id (var:id def:ast-def) ...)))

  (define-syntax-class ast-def
    #:description "ast definition"
    (pattern single:id)
    (pattern ((~datum quote) datum:id))
    (pattern (multiple:multi-ast-def ...))
    ;; (pattern (~seq repeat:ast-def (~datum (datum->syntax #f '...))))
    ;; (pattern (nest:ast-def))
    )
  (define-splicing-syntax-class multi-ast-def
    #:description "ast definition"
    (pattern ms:ast-def)
    (pattern (~seq repeat:ast-def (~datum ...)))

    ;; (pattern (nest:ast-def))
    )

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
       (let* (;; [info `((ast .
              ;;              (types . ,(syntax->datum #'(nodes.name ...)))
              ;;              ,(for/list ([names (syntax->list #'(nodes.var ... ...))]
              ;;                          [defs  (syntax->list #'(nodes.def ... ...))])
              ;;                 (print names defs)))  )]
              [base-struct-id (format-id #'cid "$~a" #'cid)]
              [base-struct  #`(struct #,base-struct-id ())])
         (for/list ([name (syntax->list #'(nodes.name ...))]
                    [vars (syntax->list #'((nodes.var ...) ...))]
                    [defs  (syntax->datum #'((nodes.def ...) ...))])
           (define group-struct-id (format-id #'cid "$~a:~a" #'cid name))
           (define group-struct #`(struct #,group-struct-id #,base-struct-id ()))
           (define var-structs
             (for/list ([var (syntax->list  vars)]
                        [def defs])
               (define var-struct-id (format-id #'cid "$~a:~a:~a" #'cid name var))

               #`(struct #,var-struct-id #,group-struct-id #,def)))
           (pretty-display (syntax->datum group-struct))
           (pretty-display (map syntax->datum var-structs)))


         ;; (print (for/list ([name (syntax->datum #'(nodes.name ...))]
         ;;                   [vars (syntax->datum #'((nodes.var ...) ...))]
         ;;                   [defs  (syntax->datum #'((nodes.def ...) ...))])
         ;;          (cons name (for/list ([var vars]
         ;;                                [def defs])
         ;;                       (cons var  def)))))
         ;; (print #'(nodes.name ...))
         #`(begin 42
                  ;; #,@(ast-defines nodes)
                  ;; #,@(language-defines langs)
                  ))])))

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
