#lang racket

(define-syntax-rule (define-compiler name x ...)
  (void))

(module+ test
  (require rackunit)
  (define-namespace-anchor anchor)
  (define-syntax-rule (check-syntax=? x y)
    (parameterize ([current-namespace
                    (namespace-anchor->namespace anchor)])
      (check-equal? (syntax->datum (expand x))
                    (syntax->datum (expand y)))))

  (check-syntax=? #'1 #'1)
  (check-syntax=? #'(define-compiler foo x) #'(void)))


(expand-syntax #'(define-compiler foo x))
