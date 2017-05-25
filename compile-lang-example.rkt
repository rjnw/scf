#lang racket

(require "compile-lang.rkt")

(define-grammar math-grammar
  (terminals
   (variable  (x))  
   (number    (n)))
  (expr (e)
   [var : ,x]
   [int : ,n]
   [+   : (+ ,e ,e)]
   [let : (let ([,x ,e] ...) ,e)]
   [if  : (if ,e ,e ,e)])
  (folds (f)
         [fold-n : (fold ,x (,e ,e) ,e)])
  (EandF (ef)
   [fold : ,f]
   [expr : ,e]
   [begin : (,ef ...)]))





