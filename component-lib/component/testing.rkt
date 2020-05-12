#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         rackunit
         "private/system.rkt")

(provide
 system-test-suite)

(begin-for-syntax
  (define-syntax-class component
    (pattern (name:id e:expr)
             #:with spec #'(list 'name (list) e))

    (pattern (name:id (dep:id ...) e:expr)
             #:with spec #'(list 'name (list 'dep ...) e))))

(define-syntax (system-test-suite stx)
  (syntax-parse stx
    [(_ name:id (component:component ...+)
        (~alt
         (~optional (~seq #:before before-e:expr) #:defaults ([before-e #'void]))
         (~optional (~seq #:after  after-e:expr)  #:defaults ([after-e  #'void]))) ...
        e:expr ...+)
     #:with system-name (format-id #'name "~a-system" #'name)
     #:with suite-name (datum->syntax #'name (symbol->string (syntax->datum #'name)))
     #'(let ([system-name (make-system (list component.spec ...))]
             [component.name #f] ...)
         (test-suite
          suite-name
          #:before
          (lambda ()
            (system-start system-name)
            (set! component.name (system-ref system-name 'component.name)) ...
            (before-e))

          #:after
          (lambda ()
            (dynamic-wind
              void
              (lambda _
                (after-e))
              (lambda _
                (system-stop system-name))))

          (test-suite
           suite-name

           e ...)))]))