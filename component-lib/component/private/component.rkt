#lang racket/base

(require racket/contract/base
         racket/generic)

(provide
 gen:component
 component?
 (contract-out
  [component-start (-> component? component?)]
  [component-stop (-> component? component?)])
 gen:wrapper-component
 wrapper-component?
 (contract-out
  [component-unwrap (-> wrapper-component? any/c)]))

(define-generics component
  (component-start component)
  (component-stop component)
  #:fallbacks
  [(define component-start values)
   (define component-stop values)])

(define-generics wrapper-component
  (component-unwrap wrapper-component))
