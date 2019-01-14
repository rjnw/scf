#lang racket

(require syntax/parse)
(provide ast-spec language-spec)

(require "ast-syntax-structs.rkt")

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
           #:attr as (cons '(common) #'v))
  (pattern (~seq (~datum #:common-mutable) v:expr)
           #:attr as (cons '(common mutable) #'v))
  (pattern (~seq (~datum #:common-auto) v:expr)
           #:attr as (cons '(common auto) #'v))
  (pattern (~seq (~datum #:common-auto-mutable) v:expr)
           #:attr as (cons '(common auto mutable) #'v))
  (pattern (~seq (~datum #:terminals) nodes:ast-node ...)
           #:attr as (cons '(terminals) (attribute nodes.spec))))
(define-syntax-class ast-group
  #:description "ast group specification"
  (pattern (name:id (~optional parent:id) nodes:ast-node ... meta:group-info ...)
           #:attr spec (ast:group #'name (attribute parent) (attribute nodes.spec) (attribute meta.as))))

(define-splicing-syntax-class ast-spec
  (pattern (~seq groups:ast-group ...)
           #:attr spec (attribute groups.spec)))

(define-syntax-class language-spec
  #:description "language specification"
  (pattern (lang:id (name:id var:id ...) ...)))
