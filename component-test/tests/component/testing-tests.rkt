#lang racket/base

(require component
         component/testing
         rackunit)

(provide
 testing-tests)

(struct app (db)
  #:transparent
  #:methods gen:component
  [(define component-start values)
   (define component-stop values)])

(struct db ()
  #:transparent
  #:methods gen:component
  [(define component-start values)
   (define component-stop values)])

(define testing-tests
  (test-suite
   "testing"

   (system-test-suite test ([app (db) app]
                            [db db])
     (test-case "instantiates and binds a system named after the suite"
       (check-true (system? test-system)))

     (test-case "binds the various components inside the suite"
       (check-true (app? app))
       (check-true (db? db))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests testing-tests))
