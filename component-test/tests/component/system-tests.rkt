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
        #:methods gen:component [])

      (struct service-b (x)
        #:methods gen:component [])

      (check-exn
       exn:fail:user?
       (lambda ()
         (make-system `((a [b] ,service-a)
                        (b [a] ,service-b)))))))

   (test-suite
    "system-start"

    (test-case "does not start outliers"
      (struct service-a ()
        #:methods gen:component [])

      (define-system test
        [sa service-a])

      (system-start test-system)
      (check-exn
       exn:fail?
       (lambda ()
         (system-ref test-system 'sa))))

    (test-case "handles startup failures gracefully"
      (define events '())

      (struct db ()
        #:methods gen:component
        [(define (component-start a-db)
           (begin0 a-db
             (set! events (cons 'db-started events))))
         (define (component-stop a-db)
           (begin0 a-db
             (set! events (cons 'db-stopped events))))])

      (struct posts (db)
        #:methods gen:component
        [(define (component-start a-posts)
           (begin0 a-posts
             (set! events (cons 'posts-started events))))
         (define (component-stop a-posts)
           (begin0 a-posts
             (set! events (cons 'posts-stopped events))))])

      (struct users (db)
        #:methods gen:component
        [(define (component-start a-users)
           (set! events (cons 'users-failed events))
           (error 'component-start "fail"))
         (define (component-stop a-users)
           a-users)])

      (struct server (db users)
        #:methods gen:component
        [(define (component-start a-server)
           (begin0 a-server
             (set! events (cons 'server-started events))))
         (define (component-stop a-server)
           (begin0 a-server
             (set! events (cons 'server-stopped events))))])

      (define-system test
        [db db]
        [posts (db) posts]
        [users (db) users]
        [server (db users) server])

      (check-exn
       #rx"fail"
       (λ () (system-start test-system)))
      (check-equal?
       (reverse events)
       '(db-started posts-started users-failed posts-stopped db-stopped))))

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
           (begin0 a-service
             (set! events (cons 'a-service-started events))))

         (define (component-stop a-service)
           (begin0 a-service
             (set! events (cons 'a-service-stopped events))))])

      (define (make-a-service db)
        (check-eq? db (system-ref test-system 'db))
        (a-service))

      (struct app ()
        #:methods gen:component
        [(define (component-start app)
           (begin0 app
             (set! events (cons 'app-started events))))

         (define (component-stop app)
           (begin0 app
             (set! events (cons 'app-stopped events))))])

      (define (make-app db a-service)
        (check-eq? db (system-ref test-system 'db))
        (check-eq? a-service (system-ref test-system 'a-service))
        (app))

      (define-system test
        [db make-db]
        [app (db a-service) make-app]
        [a-service (db) make-a-service])

      (system-start test-system)
      (check-true
       (db-running
        (system-ref test-system 'db)))

      (system-stop test-system)
      (check-equal?
       (reverse events)
       '(db-started a-service-started app-started app-stopped a-service-stopped db-stopped))

      (check-false
       (db-running
        (system-ref test-system 'db)))))

   (test-suite
    "system-replace"

    (test-case "can replace components' factories within a system"
      (struct db (env)
        #:methods gen:component [])

      (struct app (db)
        #:methods gen:component [])

      (define-system prod
        [db (lambda () (db "production"))]
        [app (db) app])

      (define test-system
        (system-replace prod-system 'db (lambda ()
                                          (db "test"))))

      (system-start prod-system)
      (system-start test-system)
      (check-equal? (db-env (system-ref prod-system 'db)) "production")
      (check-equal? (db-env (system-ref test-system 'db)) "test")))

   (test-suite
    "current-system"

    (test-case "is installed upon startup"
      (define s #f)
      (struct janitor (thd)
        #:methods gen:component
        [(define (component-start j)
           (janitor (thread
                     (lambda ()
                       (set! s (current-system))))))

         (define (component-stop j)
           (thread-wait (janitor-thd j))
           (janitor #f))])

      (struct app (janitor)
        #:methods gen:component [])

      (define-system prod
        [app (janitor) app]
        [janitor (lambda ()
                   (janitor #f))])

      (system-start prod-system)
      (system-stop prod-system)
      (check-eq? prod-system s)
      (check-false (current-system))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests system-tests))
