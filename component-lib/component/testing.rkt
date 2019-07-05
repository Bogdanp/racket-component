#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         rackunit
         "private/system.rkt")

(provide
 system-test-suite)

;; TODO: Do #:before and #:after need to install continuation barriers?
(define-syntax (system-test-suite stx)
  (define-syntax-class component
    (pattern (name:id e:expr)
             #:with spec #'(list 'name (list) e))

    (pattern (name:id (dep:id ...) e:expr)
             #:with spec #'(list 'name (list 'dep ...) e)))

  (syntax-parse stx
    [(_ name:id (component:component ...+)
        (~alt
         (~optional (~seq #:before before-e:expr) #:defaults ([before-e #'void]))
         (~optional (~seq #:after  after-e:expr)  #:defaults ([after-e  #'void]))) ...
        e:expr ...+)
     (with-syntax ([system-name (format-id #'name "~a-system" #'name)]
                   [suite-name (datum->syntax #'name (symbol->string (syntax->datum #'name)))])
       (with-syntax ([(local-definition ...)
                      (syntax/loc #'system-name
                        ((component.name (system-ref system-name 'component.name)) ...))])
         #'(let ([system-name (make-system (list component.spec ...))])
             (test-suite
              suite-name
              #:before
              (lambda ()
                (system-start system-name)
                (let (local-definition ...)
                  (before-e)))

              #:after
              (lambda ()
                (dynamic-wind
                  void
                  (lambda _
                    (let (local-definition ...)
                      (after-e)))
                  (lambda _
                    (system-stop system-name))))

              (let (local-definition ...)
                e ...)))))]))
