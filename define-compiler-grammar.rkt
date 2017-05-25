#lang racket

dg ::= (define-grammar id
        (terminals
         (terminal-id (meta-var-id ...)) ...)
        (non-terminal-id (meta-var-id ...)
         [production-id : pp-syntax]
         ...))

dl := (define-language language-id grammar-id
        language-options ...
        production-options ...)

language-options ::= #:extends language-id
                   | #:top non-terminal-id


production-options ::= ([#:+all #:-all] non-terminal-id ...)
                     | ([#:+ #:-] non-terminal-id (production-id ...))


pp-syntax ::= pp-atom | pp-mult | (pp-syntax ...) 
pp-mult   ::= pp-atom | (pp-mult   ...) | (... ...)
pp-atom   ::= symbol  | ,meta-var-id
