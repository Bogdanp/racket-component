#lang racket/base

(require racket/contract/base
         racket/list
         racket/match
         "component.rkt"
         "graph.rkt")

(provide (contract-out
          [system? (-> any/c boolean?)]
          [make-system (-> system-definition? system?)]
          [system-start (-> system? void?)]
          [system-stop (-> system? void?)]
          [system-get (-> system? symbol? component?)]))

(define-logger system)

(define system-definition?
  (listof (or/c
           (list/c symbol? any/c)
           (list/c symbol? (listof symbol?) any/c))))

(struct system (dependencies factories components))

(define (make-system spec)
  (define-values (factories dependencies)
    (for/fold ([factories (hash)]
               [dependencies (make-graph)])
              ([definition spec])
      (match definition
        [(list id e)
         (values (hash-set factories id e) dependencies)]

        [(list id dep-ids e)
         (values (hash-set factories id e)
                 (for/fold ([dependencies dependencies])
                           ([dep-id dep-ids])
                   (graph-add-edge dependencies (cons id dep-id))))]

        [else
         (error 'system-spec "bad component definition ~a" definition)])))

  (system dependencies factories (make-hasheq)))

(define (system-start s)
  (define components (system-components s))
  (define components-to-start (reverse (graph-toposort (system-dependencies s))))

  (for ([id components-to-start])
    (hash-set! components id (start-component s id))))

(define (start-component s id)
  (log-system-debug "starting component ~a" id)
  (define factory (hash-ref (system-factories s) id))
  (define dependencies (reverse (graph-ref (system-dependencies s) id)))
  (define arguments (map (lambda (id) (system-get s id)) dependencies))
  (define component (apply factory arguments))
  (component-start component))

(define (system-stop s)
  (define components (system-components s))
  (define components-to-stop (graph-toposort (system-dependencies s)))

  (for ([id components-to-stop])
    (stop-component s id)
    (hash-remove! components id)))

(define (stop-component s id)
  (log-system-debug "stopping component ~a" id)
  (define component (hash-ref (system-components s) id))
  (component-stop component))

(define (system-get system id)
  (hash-ref (system-components system) id))


(module+ test
  (require rackunit)

  (define events '())

  (struct db ()
    #:methods gen:component
    [(define (component-start db)
       (set! events (cons 'db-started events))
       db)

     (define (component-stop db)
       (set! events (cons 'db-stopped events)))])

  (define (make-db)
    (db))

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

  (define test-system
    (make-system `((db ,make-db)
                   (app [db a-service] ,make-app)
                   (a-service [db] ,make-a-service))))

  (system-start test-system)
  (system-stop test-system)
  (check-equal?
   (reverse events)
   '(db-started a-service-started app-started app-stopped a-service-stopped db-stopped))

  (struct service-a (x)
    #:methods gen:component
    [(define (component-start sa) sa)
     (define (component-stop sa) (void))])

  (struct service-b (x)
    #:methods gen:component
    [(define (component-start sb) sb)
     (define (component-stop sb) (void))])

  (define test-system-2
    (make-system `((a [b] ,service-a)
                   (b [a] ,service-b))))

  (check-exn
   exn:fail:graph:cycle?
   (lambda ()
     (system-start test-system-2))))
