#lang racket/base

(require rackunit
         rackunit/text-ui)

(require "graph-tests.rkt"
         "system-tests.rkt"
         "testing-tests.rkt")

(define all-component-tests
  (test-suite
   "component"

   graph-tests
   system-tests
   testing-tests))

(module+ main
  (run-tests all-component-tests))
