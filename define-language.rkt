#lang racket
(require (for-syntax syntax/parse))

(begin-for-syntax
  (define-splicing-syntax-class language-options
    (pattern (~seq #:extends lid:id #:top gid:id))
    (pattern (~seq #:extends lid:id) #:with gid #'#f)
    (pattern (~seq #:top gid:id)     #:with lid #'#f))

  (define-syntax-class production-options
    (pattern (#:+ gid:id (nt:id ...)) #:with op #'+)
    (pattern (#:- gid:id (nt:id ...)) #:with op #'-)
    (pattern (#:+all gids:id ...) #:with op #'+all )
    ;; (pattern ((~seq #:-all) gid:id ...) #:with op #'-all nt #'#f)
    ))

;; (define-syntax (define-language stx)
;;   (syntax-parse stx
;;     [(_ lid:id gid:id lo:language-options pr:production-options ...)
;;      #'(quote (lo.lid lo.gid pr.nt))]))

;; (define-language math L1
;;   #:top expr
;;   (#:+ expr (var int +)))

;; (define-language math L2
;;   #:extends L1
;;   #:top expr)
