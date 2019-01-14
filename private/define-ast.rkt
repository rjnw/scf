#lang racket

(require
 (for-syntax
  "ast-syntax-structs.rkt"
  "ast-syntax-class.rkt"
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
    (define (group-terminals meta-info)
      (match meta-info
        [`() #f]
        [`(((terminals) . ,terminals) . ,rst) terminals]
        [else (group-terminals (cdr meta-info))]))
    (define (group-def group-spec)
      (match-define (ast:group name parent node-specs meta-info) group-spec)
      ;; (printf "\n\ngroup: ~a\n" (syntax->datum name))
      (define args (meta-args meta-info
                              #:common-auto (λ (v) #`(#,v #:auto))
                              #:common-mutable (λ (v) #`(#,v #:mutable))
                              #:common-auto-mutable (λ (v) #`(#,v #:auto #:mutable))))
      (define parent-args (group-args group-spec))
      (define group-reader
        (let ([farg (first (generate-temporaries (list name)))])
          #`(define (#,(format-id name "$~a:~a" top name) #,farg)
              (match #,farg
                #,@(append
                    (list
                     (let ([x (first (generate-temporaries '(x)))])
                       #`(#,x #:when (#,(format-id name "~a?" (group-id group-spec))
                                      #,x) #,x)))
                    (for/list ([node-spec node-specs])

                      (match-define (ast:node node-variable node-pattern node-meta-info) node-spec)
                      ;; (printf "group-reader: node-pattern: ~a\n" node-pattern)
                      (define (node-spec-sub-calls node-pattern (repeat 0))
                        ;; TODO figure out nested repeats
                        (match node-pattern
                          [(ast:pat:single s)
                           (match repeat
                             [0 #`(#,(format-id top "$~a:~a" top (get-type s)) #,s)]
                             [1 #`(map #,(format-id top "$~a:~a" top (get-type s)) #,s)])]
                          [(ast:pat:multiple s) (map (curryr node-spec-sub-calls repeat) s)]
                          [(ast:pat:repeat s) (node-spec-sub-calls s (add1 repeat))]
                          [(ast:pat:datum s) '()]))
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
                          #,@ (flatten (node-spec-sub-calls node-pattern 0))
                          ;; #,@(map (λ (n)
                          ;;           #`(#,(format-id top "$~a:~a" top (get-type n)) #,n))
                          ;;         (node-args node-pattern))
                          )))
                    (if (group-terminals meta-info)
                        (for/list ([terminal (group-terminals meta-info)])
                          (match-define (ast:node id pat meta-info) terminal)
                          (match-define (ast:pat:single fcheck) pat)
                          #`(#,id #:when (#,fcheck #,id) #,id))
                        '())
                    (list

                     #`(else (error #,(format "error parsing ~a, given: "
                                              (symbol->string (syntax->datum (group-id group-spec))))
                                    #,farg))))))))
      ;; (pretty-display (syntax->datum group-reader))

      (define (node-def node-spec)
        (match-define (ast:node var pat meta-info) node-spec)
        (define id (node-id node-spec group-spec))
        (define writer-args  (append (map cdr parent-args) (node-args pat)))
        #`(struct #,id #,(group-id group-spec) #,(node-args pat)
            ;; #:methods gen:custom-write
            ;; ((define (write-proc struc port mode)
            ;;    (match-define (#,id #,@writer-args) struc)
            ;;    (display #,(node-pat-format var pat) port)))
            ))
      (cons
       (if parent
           #`(struct #,(group-id group-spec) #,(group-id (get-group-spec parent))
               (#,@args))
           #`(struct #,(group-id group-spec)
               (#,@args)))
       (map node-def node-specs)
       ;; (cons group-reader
       ;;       (map node-def node-specs))
       ))
    (define ret (flatten (map group-def spec)))
    (pretty-print (map syntax->datum ret))
    ret)

(define (spec->storage top ast-spec)
    (define (group-storage spec)
      (define (node-storage spec)
        (define (pattern-storage pat)
          (match pat
            [(ast:pat:single s) #`(list 'single #'#,s)]
            [(ast:pat:datum s) #`(list 'datum #,s)]
            [(ast:pat:multiple s) #`(list 'multiple #,@(map pattern-storage s))]
            [(ast:pat:repeat s) #`(list 'repeat #,(pattern-storage s))]))
        (match-define (ast:node variable pattern meta-info) spec)
        #`(list 'ast:node #'#,variable #,(pattern-storage pattern) '#,meta-info))
      (match-define (ast:group name parent nodes meta-info) spec)
      #`(list 'ast:group #'#,name #'#,parent (list #,@(map node-storage nodes)) '#,meta-info))
    (printf "spec-storage\n")
    (pretty-print ast-spec)
    (pretty-print (syntax->datum #`(cons #'#,top (list #,@(map group-storage ast-spec)))))
    #`(cons #'#,top (list #,@(map group-storage ast-spec))))

  (define (storage->spec storage)
    (define (group-spec storage)
      (define (node-spec storage)
        (define (pattern-spec storage)
          (match storage
            [`(single ,s) (ast:pat:single s)]
            [`(datum ,s) (ast:pat:datum s)]
            [`(multiple ,s ...) (ast:pat:multiple (map pattern-spec s))]
            [`(repeat ,s) (ast:pat:repeat (pattern-spec s))]))
        (match-define `(ast:node ,variable ,pat ,meta-info) storage)
        (ast:node variable (pattern-spec pat) meta-info))
      (match-define `(ast:group ,name ,parent ,nodes ,meta-info) storage)
      (ast:group name parent (map node-spec nodes) meta-info))
    (match-define `(,top ,groups ...) storage)
    (values top (map group-spec groups))))

;; TODO
;; * get parents of super group
;; * use the common attributes in writer pattern
;; * figure out the reader format with the node names

(define-syntax (define-ast stx)
  (syntax-parse stx
    [(_ cid:id gs:ast-spec)
     (define ast-spec (attribute gs.spec))
     (define struct-defs (build-defs #'cid ast-spec))
     (pretty-display ast-spec)
     (printf "struct-defs:") (pretty-print (map syntax->datum struct-defs))
     (pretty-display (map syntax->datum (flatten struct-defs)))
     #`(begin (require syntax/datum)
              (define cid #,(spec->storage #'cid ast-spec))
              #,@struct-defs)]))

(module+ test
  (require "ast-syntax-structs.rkt")
  (define-ast LC
    (expr
     [lambda ((n:terminal.sym) body:expr)]
     [letrec (((ids:terminal.sym vals:expr) ...) e:expr)]
     [app (rator:expr rand:expr)]
     [sym s:terminal.sym])
    (terminal #:terminals
              [n number?]
              [sym symbol?]))
  (printf "LC:")
  (pretty-print LC)

  )
