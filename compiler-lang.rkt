#lang racket

(require (for-syntax racket/syntax racket/pretty racket/list racket/match))

(begin-for-syntax
  (define-struct compileri (id terms groups langs) #:prefab)
  (define-struct groupi    (id vars eis) #:prefab)
  (define-struct expri     (id pat) #:prefab)
  (define-struct langi     (id parent top subs adds) #:prefab)
  (define (ti terms)
    (for/hash ([term terms])
      (match term
        [`(,term-name ,vs)
         (values term-name `(,term-name ,@vs))])))
  (define (gi expr-groups)
    (for/hash ([expr expr-groups])
      (match expr
        [`(,group-name ,vars ,exprs ...)
         (values group-name
                 (groupi group-name vars
                             (make-immutable-hash
                              (map (λ (e) (cons (car e)
                                                (expri (car e) (caddr e))))
                                   exprs))))])))
  (define (get-parent opts)
    (match opts
      [`(#:extends ,parent ,_ ...)
       parent]
      [`(,_ ,opts ...)
       (get-parent opts)]
      [`()
       (void)]))
  (define (get-top opts)
    (match opts
      [`(#:top ,top ,_ ...)
       top]
      [`(,_ ,opts ...)
       (get-top opts)]
      [`() (void)]))
  (define (get-subs opts)
    (match opts
      [`((- ,subs ...) ,_ ...) 
       subs]
      [`(,_ ,opts ...)
       (get-subs opts)]
      [`() '()]))
  (define (get-adds opts)
    (match opts
      [`((+ ,adds ...) ,_ ...) 
       adds]
      [`(,_ ,opts ...)
       (get-adds opts)]
      [`() '()]))
  (define (li langs)
    (for/hash [(lang langs)]
      (match lang
        [`(,lang-name ,opts ...)
         (define parent (get-parent opts))
         (define top (get-top opts))
         (define subs (get-subs opts))
         (define adds (get-adds opts))
         (values lang-name (langi lang-name parent top subs adds))])))
  (define (build-compiler-info stx)
    (syntax-case stx (terminals expressions languages)
      [(_ compiler-name
          (terminals terms ...)
          (expressions exprs ...)
          (languages langs ...))
       (let* ([ts (syntax->datum #'(terms ...))]
              [es (syntax->datum #'(exprs ...))]
              [ls (syntax->datum #'(langs ...))]
              [cti (ti ts)]
              [cgi (gi es)]
              [cli (li ls)])
         (compileri (syntax->datum #'compiler-name)
                   cti cgi cli))])))

(define-syntax (define-compiler stx)
  (define ci (build-compiler-info stx))
  (pretty-print ci)
  (define compiler-name (compileri-id ci))
  (define csf "~a:~a")
  (define (compiler-spec-defines ci)
    #`((define-struct #,(format-id stx csf compiler-name 'expression) ())
       (define-struct #,(format-id stx csf compiler-name 'terminal) ())
       (define-struct #,(format-id stx csf compiler-name 'language) ())))
  (define (terminal-predicates-defines term-map)
    #`(define
        #,(format-id stx "~a-terminal-predicates" compiler-name)
        (make-immutable-hash
         `#,(for/list ([term (hash-keys term-map)])
              (with-syntax* ([term (datum->syntax stx term)]
                             [term-pred (format-id stx "~a?" #'term)])
                #`(term . ,term-pred))))))
  (define (terminal-structs-defines terminals)
    (for/list ([t (hash-keys terminals)])
      (with-syntax ([struct-id (format-id stx "~a:t.~a" compiler-name t)])
        #'(define-struct struct-id (v)))))
  (define (expression-structs-defines group-map)
    (define (get-expr-fields epat)
      (match epat
        [`(,(list 'unquote s) ,pat ...)
         (cons s (get-expr-fields pat))]
        [(list 'unquote x)
         (list x)]
        [`(,y ,pat ...) #:when (symbol? y)
         (get-expr-fields pat)]
        [`(,y ,pat ...) #:when (list? y)
         (append (get-expr-fields y) (get-expr-fields pat))]
        [`() '()]))
    (append*
     (for/list ([(g-id gi) group-map])
       (for/list ([(e-id ei) (groupi-eis gi)])
         (with-syntax ([struct-id (format-id stx "~a:e.~a-~a"
                                             compiler-name g-id e-id)])
           (printf "searching for fields in pattern: ~a, ~a\n" (expri-pat ei)
                   (get-expr-fields (expri-pat ei)))
           #`(define-struct struct-id #,(get-expr-fields (expri-pat ei))))))))
  (pretty-print
   (syntax->datum
    #`(begin
        #,(terminal-predicates-defines (compileri-terms ci))
        #,@(compiler-spec-defines ci)
        #,@(terminal-structs-defines (compileri-terms ci))

        #,@(expression-structs-defines (compileri-groups ci))
        )))
  #''42)

;; ll-jit compiler-info
;; (define-struct ll-jit:terminal ())
;; (define-struct ll-terminals-variable ll-jit:terminal (x))
;; ll-jit:terminal? 

(define-compiler ll-jit
  (terminals
   (variable  (x))  
   (type-variable (g))
   (float     (f))
   (number    (n))
   (basetype  (tb))

   (llvm-contextref (lcr))
   (llvm-moduleref  (lmr))
   (llvm-typeref    (ltr))
   (llvm-valueref   (lvr)))
  #;(environment
     (type
      [,x -> ,t])
     (scope
      [,g -> ,t]))
  (expressions
   (type (t)
         [name         : ,x
                       #;(@ x type)]
         [base         : ,tb]
         [struct       : (struct (,x : ,t) ...)]
         [pointer      : (pointer ,t)]
         [function     : (,x ... -> ,x)])
   (expr (e)
         [var          : ,x
                       #;(@ x scope)]
         [float        : (fl-value ,d ,t)]
         [signed-int   : (si-value ,n ,t)]
         [unsigned-int : (ui-value ,n ,t)]
         [type         : (type ,t)
                       #;(@ t type)]
         [sizeof       : (sizeof ,expr-type)]
         [gep          : (gep ,e (,e ...))]
         [app          : (,e ,e ...)])
   (phi  (p)
         [phis         : ([,x : ,t] ...)
                       #;(@ t type)])
   (stmt (s)
         [expr         : (expr ,e)]
         [if-phi       : (if ,p ,e ,s ,s)]
         [if           : (if ,e ,s ,s)]
         [let          : (let ([,x : ,t ,e] ...) ,s)
           #;(s:scope (+ (x -> t)))]
         [while-phi    : (while ,p ,e ,s)]
         [while        : (while ,e ,s)]
         [return       : (return ,e)]
         [return-void  : (return-void)]
         [set          : (set! ,x ,e)
                       #;(@ x scope)]
         [store        : (store! ,x ,e)
                       #;(@ x scope)])
   #;(se  (se) #:reuse (stmt-while stmt-store expr-*))
   (def (d)
     [type         : (define-type ,x ,t)
                   #;(type! (x t))]
     [function     : (define-function ,x-f ((,x-a : ,t-a) ...) : ,t-r ,s)
                   #;(s:scope (+ (x-a -> t-a)
                                 (x-f -> (type-function (t-a ... -> t-r)))))
                   #;(scope! (x-f -> (type-function (t-a ... -> t-r))))])
   (mod (m)
        [module        : (module ,d ...)
          #;(pass-environment * d)]))
  
  (languages
   (LLC1 #:top mod
         (- (stmt if-phi while-phi)))
   (LLC0 #:extends LLC1
         #:top mod
         (- (stmt (if while)))))) 



