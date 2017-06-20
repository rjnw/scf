;;;; Variants with uniform meaning
(define-syntax-class init-decl
  #:attributes (internal external default)
  (pattern internal:id
           #:with external #:internal
           #:with default #'())
  (pattern (mr:maybe-renamed)
           #:with internal #'mr.internal
           #:with external #'mr.external
           #:with default #'())
  (pattern (mr:maybe-renamed default0:expr)
           #:with internal #'mr.internal
           #:with external #'mr.external
           #:with default #'(default0)))

;;;; Variants with different meaning
; A ForClause is either
;   - (bind-clause (listof identifier) syntax)
;   - (when-clause syntax)
(struct bind-clause (vars seq-expr))
(struct when-clause (guard))
 
(define-splicing-syntax-class for-clause
  #:attributes (ast)
  (pattern [var:id seq:expr]
           #:attr ast (bind-clause (list #'var) #'seq))
  (pattern [(var:id ...) seq:expr]
           #:attr ast (bind-clause (syntax->list #'(var ...))
                                   #'seq))
  (pattern (~seq #:when guard:expr)
           #:attr ast (when-clause #'guard)))


;;;; Keyword arguments for more constraints
(struct name:id super:maybe-super (field:field ...)
  (~or (~optional
         (~or (~seq #:inspector inspector:expr)
              (~seq (~and #:transparent transparent-kw))
              (~seq (~and #:prefab prefab-kw)))
         #:name "#:inspector, #:transparent, or #:prefab option")
       (~optional (~seq (~and #:mutable mutable-kw))
                  #:name "#:mutable option")
       (~optional (~seq #:super super-expr:expr)
                  #:name "#:super option")
       (~optional (~seq #:auto-value auto:expr)
                  #:name "#:auto-value option")
       (~optional (~seq #:guard guard:expr)
                  #:name "#:guard option")
       (~seq #:property prop:expr prop-val:expr)
       (~optional (~seq #:constructor-name constructor-name:id)
                  #:name "#:constructor-name option")
       (~optional
         (~seq #:extra-constructor-name extra-constructor-name:id)
         #:name "#:extra-constructor-name option")
       (~optional (~seq (~and #:omit-define-syntaxes omit-def-stxs-kw))
                  #:name "#:omit-define-syntaxes option")
       (~optional (~seq (~and #:omit-define-values omit-def-vals-kw))
                  #:name "#:omit-define-values option"))
  ...)
