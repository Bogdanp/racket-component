#lang info

(define collection 'multi)

(define deps '())
(define build-deps '("base"
                     "component-lib"
                     "rackunit-lib"))

(define update-implies '("component-lib"))
