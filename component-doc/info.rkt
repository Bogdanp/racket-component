#lang info

(define collection "component")

(define scribblings
  '(("component.scrbl" ())))

(define deps '("base"))
(define build-deps '("component-lib"
                     "db-doc"
                     "db-lib"
                     "scribble-lib"
                     "racket-doc"))
(define update-implies '("component-lib"))
