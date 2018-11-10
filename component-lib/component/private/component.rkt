#lang racket/base

(require racket/generic)

(provide gen:component
         component?
         component-start
         component-stop)

(define-generics component
  (component-start component)
  (component-stop component))
