#lang racket/base

(require component
         rackunit)

(provide system-tests)

(define system-tests
  (test-suite
   "system"

   (test-suite
    "make-system"

    (test-case "detects cycles"
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
                        (b [a] ,service-b)))))))

   (test-suite
    "system-start"

    (test-case "does not start outliers"
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

   (test-suite
    "system-{start,stop}"

    (test-case "start and stop components in the right order"
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
        (system-get test-system 'db)))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests system-tests))
