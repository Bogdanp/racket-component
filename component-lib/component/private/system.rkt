#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/contract/base
         racket/match
         "component.rkt"
         "dependency.rkt")

(provide
 (for-syntax component)

 define-system

 (contract-out
  [make-system (-> system-spec/c system?)]
  [current-system (parameter/c (or/c false/c system?))]
  [system? (-> any/c boolean?)]
  [system-start (-> system? void?)]
  [system-stop (-> system? void?)]
  [system-ref (case->
               (-> symbol? component?)
               (-> system? symbol? component?))]
  [system-get (case->
               (-> symbol? component?)
               (-> system? symbol? component?))]
  [system-replace (-> system? symbol? any/c system?)]
  [system->dot (-> system? string?)]
  [system->png (-> system? path-string? boolean?)]))

(define-logger system)

(define system-spec/c
  (listof (or/c
           (list/c symbol? any/c)
           (list/c symbol? (listof symbol?) any/c))))

(struct system (dependencies factories components))

(define current-system
  (make-parameter #f))

(begin-for-syntax
  (define-syntax-class component
    (pattern (name:id e:expr)
             #:with spec #'(list 'name (list) e))
    (pattern (name:id (dep-name:id ...) e:expr)
             #:with spec #'(list 'name (list 'dep-name ...) e))))

(define-syntax (define-system stx)
  (syntax-parse stx
    [(_ id:id component:component ...+)
     #:with full-id (format-id #'id "~a-system" #'id)
     #'(define full-id (make-system (list component.spec ...)))]))

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
  (parameterize ([current-system s])
    (for ([id (starting-order (system-dependencies s))])
      (hash-set! (system-components s) id (start-component s id)))))

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

(define system-ref
  (case-lambda
    [(id)
     (system-ref (current-system) id)]

    [(s id)
     (define components (system-components s))
     (hash-ref components id (lambda ()
                               (raise-argument-error 'system-ref "a declared component" id)))]))

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
