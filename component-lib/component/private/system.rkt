#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/contract/base
         racket/match
         "component.rkt"
         "dependency.rkt")

(provide
 define-system

 (contract-out
  [make-system (-> system-spec/c system?)]
  [system? (-> any/c boolean?)]
  [system-start (-> system? void?)]
  [system-stop (-> system? void?)]
  [system-ref (-> system? symbol? component?)]
  [system-get (-> system? symbol? component?)]
  [system-replace (-> system? symbol? any/c system?)]
  [system->dot (-> system? string?)]
  [system->png (-> system? path-string? boolean?)]))

(define-logger system)

(define system-spec/c
  (listof (or/c
           (list/c symbol? any/c)
           (list/c symbol? (listof symbol?) any/c))))

(struct system (dependencies factories components))

(define-syntax (define-system stx)
  (define-syntax-class component
    (pattern (name:id e:expr)
             #:with spec #'(list 'name (list) e))
    (pattern (name:id (dep-name:id ...) e:expr)
             #:with spec #'(list 'name (list 'dep-name ...) e)))

  (syntax-parse stx
    [(_ name:id component0:component component:component ...)
     (with-syntax ([name (format-id #'name "~a-system" #'name)])
       #'(define name
           (make-system (list component0.spec component.spec ...))))]))

(define (make-system spec)
  (define-values (factories dependencies)
    (for/fold ([factories (hash)]
               [dependencies (make-dependency-graph)])
              ([definition spec])
      (match definition
        [(list id e)
         (values (hash-set factories id e) dependencies)]

        [(list id dep-ids e)
         (values (hash-set factories id e)
                 (for/fold ([dependencies dependencies])
                           ([dep-id dep-ids])
                   (depend dependencies id dep-id)))]

        [else
         (error 'make-system "bad component definition ~a" definition)])))

  (system dependencies factories (make-hasheq)))

(define (system-start s)
  (log-system-debug "starting system")
  (for ([id (starting-order (system-dependencies s))])
    (hash-set! (system-components s) id (start-component s id))))

(define (start-component s id)
  (log-system-debug "starting component ~a" id)
  (define factory (hash-ref (system-factories s) id))
  (define dependencies (direct-dependencies (system-dependencies s) id))
  (define arguments (map (lambda (id) (system-ref s id)) dependencies))
  (define component (apply factory arguments))
  (component-start component))

(define (system-stop s)
  (log-system-debug "stopping system")
  (for ([id (stopping-order (system-dependencies s))])
    (hash-set! (system-components s) id (stop-component s id))))

(define (stop-component s id)
  (log-system-debug "stopping component ~a" id)
  (component-stop (system-ref s id)))

(define (system-ref s id)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (raise-argument-error 'system-ref "a declared component" id))])
    (hash-ref (system-components s) id)))

(define system-get system-ref)  ;; backwards-compat

(define (system-replace s id factory)
  (define factories (system-factories s))
  (unless (hash-has-key? factories id)
    (raise-argument-error 'system-ref "a declared component" id))

  (struct-copy system s
               [components (make-hash)]
               [factories (hash-set factories id factory)]))

(define (system->dot s)
  (dependency-graph->dot (system-dependencies s)))

(define (system->png s output-path)
  (dependency-graph->png (system-dependencies s) output-path))
