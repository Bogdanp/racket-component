#lang racket/base

(require racket/contract/base
         racket/generic)

(provide gen:component
         component?
         (contract-out
          [component-start (-> component? component?)]
          [component-stop (-> component? component?)]))

(define-generics component
  (component-start component)
  (component-stop component)
  #:fallbacks
  [(define component-start values)
   (define component-stop values)])
