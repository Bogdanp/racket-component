#lang info

(define license 'BSD-3-Clause)
(define collection "component")

(define scribblings
  '(("component.scrbl" ())))

(define deps '("base"))
(define build-deps '("component-lib"
                     "db-doc"
                     "db-lib"
                     "scribble-lib"
                     "racket-doc"
                     "rackunit-doc"
                     "rackunit-lib"))
(define update-implies '("component-lib"))
