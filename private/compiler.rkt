#lang racket

(module ast-syntax-structs racket
  (provide (all-defined-out))

  (struct ast:group (name parent nodes meta-info) #:prefab)
  (struct ast:node (variable pattern meta-info) #:prefab)
  (struct ast:pat:single   (spec) #:prefab)
  (struct ast:pat:datum    (spec) #:prefab)
  (struct ast:pat:multiple (spec) #:prefab)
  (struct ast:pat:repeat   (spec) #:prefab))

(module ast-stxclass racket
  (require syntax/parse)
  (provide ast-spec language-spec)
  (require (submod ".." ast-syntax-structs))

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
    (pattern (lang:id (name:id var:id ...) ...))))

(module definer racket
  (require (for-syntax (submod ".." ast-stxclass)
                       (submod ".." ast-syntax-structs)
                       syntax/parse
                       racket/syntax
                       racket/pretty))

  (provide define-ast)

  (begin-for-syntax
    (require racket)
    (require syntax/datum)
    (define (full-id lineage)
      (foldl (λ (c d) (format-id c "~a:~a" c d)) (car lineage) (cdr lineage)))
    (define (node-args node-pat)
      (define (rec pat)
        (match pat
          [(ast:pat:single s)  s]
          [(ast:pat:multiple m) (map rec m)]
          [(ast:pat:repeat r) (rec r)]
          [(ast:pat:datum _) '()]))
      (flatten (rec node-pat)))
    (define (strip-single s)
      (define splt (string-split (symbol->string (syntax->datum s)) ":"))
      (datum->syntax s (string->symbol (car splt))))
    (define (get-type s)
      (define ss (symbol->string (syntax->datum s)))
      (define t (string-split (second (string-split ss ":")) "."))
      (datum->syntax s (string->symbol (first t))))

    (define (info-args meta-info)
      (match meta-info
        ['() '()]
        [`(((common) . ,c) . ,rst) (cons (car meta-info) (info-args rst))]
        [`(((common auto) . ,c) . ,rst) (cons (car meta-info) (info-args rst))]
        [`(((common mutable) . ,c) . ,rst) (cons (car meta-info) (info-args rst))]
        [`(((common auto mutable) . ,c) . ,rst) (cons (car meta-info) (info-args rst))]
        [else (info-args (cdr meta-info))]))

    (define (meta-args meta-info
                       #:common (cc identity)
                       #:common-mutable (cm identity)
                       #:common-auto (ca identity)
                       #:common-auto-mutable (cam identity))
      (define (rec mi)
        (match mi
          ['() '()]
          [`(((common) . ,c) . ,rst) (cons (cc c) (rec rst))]
          [`(((common mutable) . ,c) . ,rst) (cons (cm c) (rec rst))]
          [`(((common auto) . ,c) . ,rst) (cons (ca c) (rec rst))]
          [`(((common auto mutable) . ,c) . ,rst) (cons (cam c) (rec rst))]
          [else (meta-args (cdr meta-info))]))
      (rec meta-info))

    (define (node-pat-format var node-pat) ;; TODO: cleanup
      (define (build-repeat-printer lst)
        (if (list? lst)
            (let ([g-vars (map (λ(v) (gensym 'g)) lst)])
              #`,@(with-datum #,(for/list ([r lst]
                                           [g g-vars])
                                  #`(( #,g #,(datum->syntax #'42 '...)) `#,(build-printer r)))
                    (datum (#,g-vars  #,(datum->syntax #'42 '...)))))
            #`,@`#,(build-printer lst)))
      (define (build-printer v)
        (if (syntax? v)
            v
            (if (ast:pat:single? v)
                #`,#,(ast:pat:single-spec v)
                ;;hack to pass the pattern itself for distinguishing repeat and single
                #`(#,@(map build-printer v)))))
      (define (rec pat)
        (match pat
          [(ast:pat:single s) pat]
          [(ast:pat:multiple m) (map rec m)]
          [(ast:pat:repeat r) (build-repeat-printer (rec r))]
          [(ast:pat:datum d) d]))
      (define p (build-printer (rec node-pat)))
      (if (ast:pat:multiple? node-pat)
          #``(#,var ,@`#,p)
          #``#,p))

    (define (node-reader-pattern pattern)
      (match pattern
        [(ast:pat:single s)  #`,#,s]
        [(ast:pat:multiple m)
         #`(#,@(foldr (λ (p v)
                        (match p
                          [(ast:pat:repeat r) (append (node-reader-pattern p) v)]
                          [else (cons (node-reader-pattern p) v)]))
                      '()
                      m))]
        [(ast:pat:repeat r)
         (list (node-reader-pattern r) (datum->syntax #'42 '...))]
        [(ast:pat:datum d) d]))

    (define (build-defs top spec)
      (define (build-group-map spec)
        (match spec
          [`(,groups ...)
           (map build-group-map groups)]
          [(ast:group name _ _ meta-info)
           (cons (syntax->datum name) spec)]))
      (define group-map (make-hash (build-group-map spec)))
      (define (get-group-spec group-id)
        (if group-id (hash-ref group-map (syntax->datum group-id)) #f))
      (define (group-id spec)
        (match spec
          [#f top]
          [(ast:group name parent _ _)
           (format-id top "~a:~a" (group-id (get-group-spec parent)) name)]))
      (define (node-id node-spec group-spec)
        (match-define (ast:node var pat meta-info) node-spec)
        (format-id var "~a:~a" (group-id group-spec) var))
      (define (group-args group-spec)
        (match-define (ast:group id parent node meta-info) group-spec)
        (append (if parent (group-args (hash-ref group-map (syntax->datum parent))) empty)
                (info-args meta-info)))
      (define (group-def group-spec)
        (match-define (ast:group name parent node-specs meta-info) group-spec)
        ;; (printf "\n\ngroup: ~a\n" (syntax->datum name))
        (define args (meta-args meta-info
                                #:common-auto (λ (v) #`(#,v #:auto))
                                #:common-mutable (λ (v) #`(#,v #:mutable))
                                #:common-auto-mutable (λ (v) #`(#,v #:auto #:mutable))))
        ;; (printf "meta-args: ~a\n" args)
        (define parent-args (group-args group-spec))
        ;; (printf "parent-args: ~a\n" parent-args)
        (define group-reader
          (let ([farg (first (generate-temporaries (list name)))])
            #`(define (#,(format-id name "$~a:~a" top name) #,farg)
                (match #,farg
                  #,@(append
                      (for/list ([node-spec node-specs])
                        (match-define (ast:node node-variable node-pattern node-meta-info) node-spec)
                        (define clean-parent-args
                          (map (compose strip-single cdr)
                               (filter (λ (v) (not (member 'auto (car v)))) parent-args)))
                        (define match-pat
                          (match node-pattern
                            [(ast:pat:single s) s]
                            [else
                             #``(#,node-variable
                                 #,@(map (λ (v) #`,#,v) clean-parent-args)
                                 #,@(node-reader-pattern node-pattern))]))
                        #`(#,match-pat
                           (#,(node-id node-spec group-spec)
                            #,@clean-parent-args
                            #,@(map (λ (n)
                                      #`(#,(format-id top "$~a:~a" top (get-type n)) #,n))
                                    (node-args node-pattern)))))
                      (list #`(else #,farg)))))))
        ;; (printf "group-reader:\n" )(pretty-display (syntax->datum group-reader))
        (define (node-def node-spec)
          (match-define (ast:node var pat meta-info) node-spec)
          (define id (node-id node-spec group-spec))
          (define writer-args  (append (map cdr parent-args) (node-args pat)))
          #`(struct #,id #,(group-id group-spec) #,(node-args pat)
              #:methods gen:custom-write
              ((define (write-proc struc port mode)
                 (match-define (#,id #,@writer-args) struc)
                 (display #,(node-pat-format var pat) port)))))
        (cons
         (if parent
             #`(struct #,(group-id group-spec) #,(group-id (get-group-spec parent))
                 (#,@args))
             #`(struct #,(group-id group-spec)
                 (#,@args)))
         (cons group-reader
               (map node-def node-specs))))
      (define ret (flatten (map group-def spec)))
      ;; (pretty-print (map syntax->datum ret))
      ret))

  ;; TODO
  ;; * get parents of super group
  ;; * use the common attributes in writer pattern
  ;; * figure out the reader format with the node names

  (define-syntax (define-ast stx)
    (syntax-parse stx
      [(_ cid:id gs:ast-spec)
       (define ast-spec (attribute gs.spec))
       (define struct-defs (build-defs #'cid ast-spec))
       (printf "struct-defs: ~a\n" struct-defs)
       (pretty-display (map syntax->datum (flatten struct-defs)))
       #`(begin (require syntax/datum) #,@struct-defs)])))

(require (submod "." definer))
(provide define-ast)
