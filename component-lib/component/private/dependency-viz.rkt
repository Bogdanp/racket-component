#lang racket/base

(require racket/contract/base
         racket/match
         racket/port
         racket/system
         "graph.rkt")

(provide (contract-out
          [dependency-graph->dot (->* (graph?) (#:name string?) string?)]
          [dependency-graph->png (->* (graph? path-string?) (#:name string?) boolean?)]))

(define (dependency-graph->dot dg #:name [name "G"])
  (with-output-to-string
    (lambda ()
      (displayln (format "digraph ~a {" name))
      (for ([edge (graph-edges dg)])
        (displayln (format "  ~a -> ~a;" (car edge) (cdr edge))))
      (displayln "}"))))

(define (dependency-graph->png dg output-path #:name [name "G"])
  (define filename (path->string (build-path output-path)))
  (match (process* (find-executable-path "dot") "-Tpng" "-o" filename)
    [(list stdout stdin pid stderr control)
     (display (dependency-graph->dot dg) stdin)
     (close-output-port stdin)
     (control 'wait)
     (= 0 (control 'exit-code))]))
