#lang racket
(module compiler-define-kws racket
  (define-syntax terminals (syntax-rules ()))
  (define-syntax expressions (syntax-rules ()))
  (define-syntax languages (syntax-rules ()))
  (provide terminals expressions languages))

(module compiler-stxclass racket
  (require syntax/parse
           (for-template (submod ".." compiler-define-kws)))
  (provide spec)
  (define-syntax-class spec
    #:literals (terminals expressions languages)
    #:attributes (i)
    #:description "specification"
    (pattern (terminals s:expr ...)
             #:attr i #'terminals)
    (pattern (expressions s:expr ...)
             #:attr i #'expressions)
    (pattern (languages s:expr ...)
             #:attr i #'languages)))

(module definer racket
  (require (for-syntax (submod ".." compiler-stxclass)
                       syntax/parse
                       racket/pretty))
  (provide define-compiler)
  (define-syntax (define-compiler stx)
    (define (compiler-info-defines)
      #'())
    (define (terminal-defines)
      #'())
    (define (expression-defines)
      #'())
    (define (language-defines)
      #'())
    (syntax-parse stx
      [(_ cid:id specs:spec ...)
       #`(begin
           #,@(compiler-info-defines)
           #,@(terminal-defines)
           #,@(expression-defines)
           #,@(language-defines))])))

(require 'definer 'compiler-define-kws)
(provide define-compiler)

(module test racket
  (require (submod ".." definer)
           (submod ".." compiler-define-kws))
  (define-compiler c0)
  (define-compiler c1 (terminals))
  (define-compiler c2 (terminals) (expressions))
  (define-compiler c3 (terminals) (expressions) (languages)))


