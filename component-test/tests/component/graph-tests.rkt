#lang racket/base

(require component/private/graph
         rackunit)

(provide graph-tests)

(define (symbol<=? x . xs)
  (or (apply eq? x xs)
      (apply symbol<? x xs)))

(define (pair<=? a-p b-p)
  (and (symbol<=? (car a-p) (car b-p))
       (symbol<=? (cdr a-p) (cdr b-p))))

(define graph-tests
  (test-suite
   "graph"

   (test-suite
    "graph-edges"

    (test-case "lists all edges in a graph"
      (check-equal?
       (sort (graph-edges (make-graph '((a . b) (a . c) (b . c)))) pair<=?)
       '((a . b)
         (a . c)
         (b . c)))))

   (test-suite
    "graph-remove-edge"

    (test-case "removes edges from a graph"
      (check-equal?
       (graph-edges (graph-remove-edge (make-graph '((a . b) (a . c))) 'a 'c))
       '((a . b)))))

   (test-suite
    "graph-toposort"

    (test-case "does nothing given an empty graph"
      (check-equal?
       (graph-toposort (make-graph '()))
       '()))

    (test-case "does nothing given a graph with one edge"
      (check-equal?
       (graph-toposort (make-graph '((a . b))))
       '(a b)))

    (test-case "sorts graphs topographically"
      (check-not-false
       (member
        (graph-toposort (make-graph '((a . b) (c . d))))
        '((a b c d)
          (c d a b))))

      (check-not-false
       (member
        (graph-toposort (make-graph '((a . b) (a . c) (b . d) (d . e))))
        '((a b d e c)
          (a c b d e))))

      (check-equal?
       (graph-toposort (make-graph '((a . b) (a . c) (a . d) (b . c) (b . d) (c . d))))
       '(a b c d)))

    (test-case "detects cycles"
      (check-exn
       exn:fail:graph:cycle?
       (lambda ()
         (graph-toposort (make-graph '((a . b) (b . a))))))))))

(module+ test
  (require rackunit/text-ui)
  (run-tests graph-tests))
