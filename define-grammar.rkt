#lang racket

(require (for-syntax racket/syntax syntax/parse racket/pretty racket/match))


(begin-for-syntax
  (struct terminali (id metavars) #:prefab)
  (struct productioni (id children) #:prefab)
  (struct variablei (id type) #:prefab)
  (struct groupi (id prods) #:prefab)
  (define (create-productioni n children types)
    (unless (eq? (length children) (length types))
      (error "unmatched types"))
    (productioni n
                 (for/list
                     ([c children]
                      [t types])
                   (variablei c t))))
  (define-syntax-class terminal
    (pattern (name:id (metavars:id ...))
             #:attr info (terminali #'name (syntax->list #'(metavars ...)))))
  (define-splicing-syntax-class tpat
    (pattern (~seq t:id) #:attr multiple #'#f)
    (pattern (~seq (* t:id)) #:attr multiple #'#t))
  (define-syntax-class production
    (pattern (name:id (children:id ...) (types:tpat ...))
             #:attr info
             (create-productioni #'name (syntax->list #'(children ...)) (syntax->list #'(types ...)))))
  (define-syntax-class group
    (pattern (name:id prods:production ...)
             #:attr info (groupi #'name (attribute prods.info))))
  )

(define-syntax (define-grammar stx)
  (syntax-parse stx #:datum-literals (terminals)
    [(_ gid:id
        (terminals terms:terminal ...)
        groups:group ...)
     (define groups-info (attribute groups.info))
     (define all-fs (for/list ([grp groups-info])
                      (format-id stx "f-~a" (groupi-id grp))))
     (define production-records
       #`(;; (define-generics functor
          ;;   (fmap f fmap))
          #,@(for/list ([grp groups-info])
          (define lgi (format-id stx "~a" (groupi-id grp)))
          (define gen-lgi (format-id stx "~ag" lgi))
          (define mpi (format-id stx "map-~a" (groupi-id grp)))
          (define mpis (format-id stx "map-~a/state" (groupi-id grp)))
          (define fn-e 'e^)
          (define fn-s 's^)

          #`(begin
              (define-generics #,gen-lgi
                (#,mpi #,@all-fs #,gen-lgi))
              (struct #,lgi ())
              #,@(for/list ([prd (groupi-prods grp)])
                   (define lgpi (format-id stx "~a-~a" lgi (productioni-id prd)))
                   (define vars (map variablei-id (productioni-children prd)))
                   (define (figure-fn c st)
                     (match (syntax->datum (variablei-type c))
                       [`((* ,t)) #`(map #,(format-id stx st t) #,(variablei-id c))]
                       [`(,t) #`(#,(format-id stx st t)  #,(variablei-id c))]))
                   #`(struct
                       #,lgpi
                       #,lgi
                       (#,@vars)
                       #:methods #,(format-id stx "gen:~a" gen-lgi)
                       ((define (#,mpi #,@all-fs #,fn-e)
                          (match-define (#,lgpi #,@vars) #,fn-e)
                          (#,lgpi #,@(for/list ([c (productioni-children prd)])
                                       (figure-fn c "f-~a"))))
                        ;; (define (#,mpis #,@all-fs #,fn-e #,fn-s)
                        ;;   (match-define (#,lgpi #,@vars) #,fn-e)
                        ;;   (#,lgpi #,@(for/list ([c (productioni-children prd)])
                        ;;                (figure-fn c (format "(curryr f-~~a ~a)" fn-s)))))
                        )))))))
     (define pass-syntax
       #`(begin
           (define-syntax (create-pass stx)
             (syntax-case stx #,(map groupi-id groups-info)
               [#,(cons #'_
                        (for/list ([grp groups-info])
                          #`(#,(groupi-id grp) #,(format-id stx "mat-~a" (groupi-id grp)) (... ...))))
                (#,(format-id stx "quasisyntax")
                 (letrec #,(for/list ([grp groups-info])
                             #`(#,(format-id stx "f-~a" (groupi-id grp))
                                (λ (e)
                                  (define ne
                                    (match e
                                      #,(format-id stx "mat-~a" (groupi-id grp))
                                      (... ...)
                                      [else e]))
                                   (#,(format-id stx "map-~a" (groupi-id grp)) #,@all-fs ne))))
                   (λ (e) (map-expr #,@all-fs e))))]))
           (define-syntax (create-rpass stx)
             (syntax-case stx #,(map groupi-id groups-info)
               [#,(cons #'_(for/list ([grp groups-info])
                             #`(#,(groupi-id grp) #,(format-id stx "mat-~a" (groupi-id grp)) (... ...))))
                (#,(format-id stx "quasisyntax")
                 (letrec #,(for/list ([grp groups-info])
                             #`(#,(format-id stx "f-~a" (groupi-id grp))
                                (λ (e) (define ne (#,(format-id stx "map-~a" (groupi-id grp)) #,@all-fs e))
                                   (match ne
                                         #,(format-id stx "mat-~a" (groupi-id grp))
                                         (... ...)
                                         [else ne]))))
                   (λ (e) (map-expr #,@all-fs e))))]))
           )
       ;; (for/list ([grp groups-info])
       ;;   #`(define-syntax (expr/pass stx)
       ;;       (syntax-case stx ()
       ;;           [(_ mps (... ...))
       ;;            (λ (e)
       ;;              (match (match e mps (... ...) [else e])
       ;;                [e e]))])))
       )

(parameterize ([pretty-print-current-style-table
                         (pretty-print-extend-style-table
                          (pretty-print-current-style-table)
                          '(block define-variables define-function assign while)
                          '(begin let lambda set! do))]
                        [pretty-print-columns 80])
     (pretty-print `(begin
                      ,@(syntax->datum production-records)))
     (pretty-print (syntax->datum pass-syntax)))

     #`(begin #,@production-records
              #,pass-syntax
              )
     ;; (pretty-print
     ;;  (syntax->datum
     ;;   #`(begin
     ;;       (define gid
     ;;         (grammeri gid (list terms.name ... ))
     ;;         terms ...
     ;;         ))))
]))
(require racket/generic)
(define map-symbol identity)
(define mapr-symbol identity)
(define super-map-symbol identity)
(define super-mapr-symbol identity)
(define-grammar hakaru
  (terminals
   (int (i))
   (nat (n))
   (float (f))
   (symbol (s))
   (prob (p)))
  (expr
   (mod (main fns) [expr (* expr)])
   (fun (args ret-type body) [(* expr) symbol expr])
   (let (type var val body) [symbol expr expr expr])
   (lets (types vars vals body) [symbol (* expr) (* expr) expr])
   (var (type sym orig) [symbol symbol symbol])
   (arr (type index size body) [symbol expr expr expr])
   (sum (type index start end body) [symbol expr expr expr expr])
   (prd (type index start end body) [symbol expr expr expr expr])
   (bucket (type start end reducer) [symbol expr expr reducer])
   (branch (pat body) (pat expr))
   (match (type tst branches) [symbol expr (* expr)])
   (bind (var body) (expr expr))
   (if (type tst thn els) (symbol expr expr expr))
   (app (type rator rands) (symbol expr (* expr)))
   (val (type v) (symbol symbol))
   (intr (sym) (symbol))
   (intrf (sym) (symbol))
   (block (type stmt body) (symbol stmt expr)))
  (reducer
   (split (e a b) [expr reducer reducer])
   (fanout (a b) [reducer reducer])
   (add (e) [expr])
   (nop () ())
   (index (n i a) (expr expr reducer)))
  (stmt
   (lets (types vars bstmt) (symbol (* expr) stmt))
   (return (val) (expr))
   (if (tst thn els) (expr stmt stmt))
   (for (i start end body) (expr expr expr stmt))
   (block (stmts) ((* stmt)))
   (assign (var val) (expr expr))
   (void () ()))
  (pat
   (true () ())
   (false () ())
   (pair (a b) [pat pat])
   (var () ())
   (ident () ())))
