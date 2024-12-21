#lang racket/base

(require review/ext
         syntax/parse/pre)

#|review: ignore|#

(provide
 should-review-syntax?
 review-syntax)

(define (should-review-syntax? stx)
  (syntax-case stx (define-system)
    [(define-system . _rest) #t]
    [_ #f]))

(define-syntax-class component-spec
  (pattern [component-id:id
            {~optional (dependency:id ...)}
            {~do (push-scope)}
            _factory-expr:expression
            {~do (pop-scope)}]
           #:attr deps #'{~? (dependency ...) ()}))

(define-syntax-class system-definition
  #:datum-literals (define-system)
  (pattern (define-system system-id:id
             spec:component-spec ...+)
           #:do [(track-binding #'system-id "~a-system")
                 (define all-components
                   (for/list ([id-stx (in-list (syntax-e #'(spec.component-id ...)))])
                     (syntax-e id-stx)))
                 (for* ([deps-stx (in-list (syntax-e #'(spec.deps ...)))]
                        [dep-stx (in-list (syntax-e deps-stx))])
                   (unless (memq (syntax-e dep-stx) all-components)
                     (track-warning dep-stx (format "component ~a is not defined" (syntax-e dep-stx)))))]))

(define (review-syntax stx)
  (syntax-parse stx
    [s:system-definition #'s]
    [_ (track-error stx "expected a system definition")]))
