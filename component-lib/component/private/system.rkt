#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/contract/base
         racket/match
         "component.rkt"
         "dependency.rkt")

(provide define-system

         (contract-out
          [system? (-> any/c boolean?)]
          [make-system (-> system-spec? system?)]
          [system-start (-> system? void?)]
          [system-stop (-> system? void?)]
          [system-get (-> system? symbol? component?)]
          [system->dot (-> system? string?)]
          [system->png (-> system? path-string? string?)]))

(define-logger system)

(define system-spec?
  (listof (or/c
           (list/c symbol? (-> any/c component?))
           (list/c symbol? (listof symbol?) (-> any/c component?)))))

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
         (error 'system-spec "bad component definition ~a" definition)])))

  (system dependencies factories (make-hasheq)))

(define (system-start s)
  (log-system-debug "starting system")
  (for ([id (starting-order (system-dependencies s))])
    (hash-set! (system-components s) id (start-component s id))))

(define (start-component s id)
  (log-system-debug "starting component ~a" id)
  (define factory (hash-ref (system-factories s) id))
  (define dependencies (direct-dependencies (system-dependencies s) id))
  (define arguments (map (lambda (id) (system-get s id)) dependencies))
  (define component (apply factory arguments))
  (component-start component))

(define (system-stop s)
  (log-system-debug "stopping system")
  (for ([id (stopping-order (system-dependencies s))])
    (hash-set! (system-components s) id (stop-component s id))))

(define (stop-component s id)
  (log-system-debug "stopping component ~a" id)
  (define component (hash-ref (system-components s) id))
  (component-stop component))

(define (system-get s id)
  (with-handlers ([exn:fail? (lambda (e)
                               (raise-argument-error 'system-get "a declared component" id))])
    (hash-ref (system-components s) id)))

(define (system->dot s)
  (dependency-graph->dot (system-dependencies s)))

(define (system->png s output-path)
  (dependency-graph->png (system-dependencies s) output-path))


(module+ test
  (require rackunit)

  (test-case "components are started and stopped in the right order"
    (define events '())

    (struct db (running)
      #:methods gen:component
      [(define (component-start a-db)
         (set! events (cons 'db-started events))
         (struct-copy db a-db [running #t]))

       (define (component-stop a-db)
         (set! events (cons 'db-stopped events))
         (struct-copy db a-db [running #f]))])

    (define (make-db)
      (db #f))

    (struct a-service ()
      #:methods gen:component
      [(define (component-start a-service)
         (set! events (cons 'a-service-started events))
         a-service)

       (define (component-stop a-service)
         (set! events (cons 'a-service-stopped events))
         a-service)])

    (define (make-a-service db)
      (check-eq? db (system-get test-system 'db))
      (a-service))

    (struct app ()
      #:methods gen:component
      [(define (component-start app)
         (set! events (cons 'app-started events))
         app)

       (define (component-stop app)
         (set! events (cons 'app-stopped events))
         app)])

    (define (make-app db a-service)
      (check-eq? db (system-get test-system 'db))
      (check-eq? a-service (system-get test-system 'a-service))
      (app))

    (define-system test
      [db make-db]
      [app (db a-service) make-app]
      [a-service (db) make-a-service])

    (system-start test-system)
    (check-true
     (db-running
      (system-get test-system 'db)))

    (system-stop test-system)
    (check-equal?
     (reverse events)
     '(db-started a-service-started app-started app-stopped a-service-stopped db-stopped))

    (check-false
     (db-running
      (system-get test-system 'db))))

  (test-case "cycles are detected immediately"
    (struct service-a (x)
      #:methods gen:component
      [(define (component-start sa) sa)
       (define (component-stop sa) sa)])

    (struct service-b (x)
      #:methods gen:component
      [(define (component-start sb) sb)
       (define (component-stop sb) sb)])

    (check-exn
     exn:fail:user?
     (lambda ()
       (make-system `((a [b] ,service-a)
                      (b [a] ,service-b))))))

  (test-case "outliers are not started"
    (struct service-a ()
      #:methods gen:component
      [(define (component-start sa) sa)
       (define (component-stop sa) sa)])

    (define-system test
      [sa service-a])

    (system-start test-system)
    (check-exn
     exn:fail?
     (lambda ()
       (system-get test-system 'sa)))))
