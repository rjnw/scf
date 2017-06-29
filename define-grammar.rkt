#lang racket

(require (for-syntax racket/syntax syntax/parse racket/pretty racket/match))


(begin-for-syntax
  (struct terminali (id metavars) #:prefab)
  (struct productioni (id children) #:prefab)
  (struct variablei (id type) #:prefab)
  (struct groupi (id prods) #:prefab)
  (define (create-productioni n children types)
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
     (define all-fs (cons #'f-terminal
                          (for/list ([grp groups-info])
                            (format-id stx "f-~a" (groupi-id grp)))))
     (define production-records
       (for/list ([grp groups-info])
         (define lgi (format-id stx "~a:~a" #'gid  (groupi-id grp)))
         (define gen-lgi (format-id stx "gen:~a" lgi))
         (define map-id (format-id stx "map-~a" (groupi-id grp)))
         (define mapr-id (format-id stx "mapr-~a" (groupi-id grp)))
         (define fn-e 'e^;; (gensym 'e)
           )
         #`(begin
             (struct #,lgi ())
            (define-generics #,gen-lgi
              (#,map-id #,@all-fs #,(groupi-id grp))
              (#,mapr-id #,@all-fs #,(groupi-id grp)))
            #,@(for/list ([prd (groupi-prods grp)])
                 (define lgpi (format-id stx "~a:~a" lgi (productioni-id prd)))
                 (define vars (map variablei-id (productioni-children prd)))
                 (define (figure-map c)
                   (match (syntax->datum (variablei-type c))
                     [`((* ,t)) #`(map #,(format-id stx "map-~a" t) all-fns #,(variablei-id c))]
                     [`(,t) #`(#,(format-id stx "map-~a" t) all-fns #,(variablei-id c))]))
                 #`(struct
                     #,lgpi
                     #,lgi
                     (#,@vars)
                     #:methods #,gen-lgi
                     ((define (#,map-id #,@all-fs #,fn-e)
                        (match-define (#,lgpi #,@vars)
                          (#,(format-id stx "f-~a" (groupi-id grp)) #,fn-e))
                        (#,lgpi #,@(for/list ([c (productioni-children prd)])
                                     (figure-map c))))
                      (define (#,mapr-id #,@all-fs #,fn-e)
                        (match-define (#,lgpi #,@vars) #,fn-e)
                        (#,(format-id stx "f-~a" (groupi-id grp))
                         (#,lgpi #,@(for/list ([c (productioni-children prd)])
                                      (figure-map c)))))))))))
     (define pass-syntax
       #`(define-syntax (lang/pass stx)
           (syntax-case stx #,(map groupi-id groups-info)
             [#,(for/list ([grp groups-info])
                  #`(#,(groupi-id grp) #,(format-id stx "mat-~a" (groupi-id grp)) (... ...)))
              (letrec #,(for/list ([grp groups-info])
                          #`(#,(format-id stx "f-~a" (groupi-id grp))
                             (λ (e) (match e
                                      #,(format-id stx "mat-~a" (groupi-id grp))
                                      (... ...)
                                      [else e]))))
                (map-<top> #,@all-fs e))]))
       ;; (for/list ([grp groups-info])
       ;;   #`(define-syntax (expr/pass stx)
       ;;       (syntax-case stx ()
       ;;           [(_ mps (... ...))
       ;;            (λ (e)
       ;;              (match (match e mps (... ...) [else e])
       ;;                [e e]))])))
       )

     (pretty-print `(begin
                      ,@(map syntax->datum production-records)))
     (pretty-print (syntax->datum pass-syntax))
     ;; (pretty-print
     ;;  (syntax->datum
     ;;   #`(begin
     ;;       (define gid
     ;;         (grammeri gid (list terms.name ... ))
     ;;         terms ...
     ;;         ))))
     #''42]))

(define-grammar hakaru
  (terminals
   (int (i))
   (nat (n))
   (float (f))
   (symbol (s))
   (prob (p)))
  (expr
   (mod (main fns) [expr (* expr)])
   (fun (args ret-type body) [(* symbol) symbol expr])
   (let (type var val body) [symbol expr expr expr]))
  (stmt
   (if (tst thn els) (expr stmt stmt))
   (for (i start end body) (expr expr expr stmt)))
  (pat
   (true () ())
   (false () ())
   (pair (a b) [pat pat]))
  (rdcr
   (split (e a b) [expr rdcr rdcr])
   (fanout (a b) [rdcr rdcr])
   (add (e) [expr])))
