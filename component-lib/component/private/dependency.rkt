#lang racket/base

;; Provides a directed acyclic graph data structure suitable for
;; building dependency graphs.

(require racket/contract/base
         racket/match
         racket/port
         racket/set
         racket/system
         "graph.rkt")

(provide (contract-out
          [make-dependency-graph (->* () ((listof pair?)) graph?)]
          [depends? (-> graph? any/c any/c boolean?)]
          [depend (-> graph? any/c any/c graph?)]
          [direct-dependencies (-> graph? any/c (listof any/c))]
          [transitive-dependencies (-> graph? any/c (listof any/c))]
          [starting-order (-> graph? (listof any/c))]
          [stopping-order (-> graph? (listof any/c))]
          [dependency-graph->dot (->* (graph?) (#:name string?) string?)]
          [dependency-graph->png (->* (graph? path-string?) (#:name string?) boolean?)]))

(define (make-dependency-graph [deps '()])
  (for/fold ([graph (make-graph)])
            ([dep deps])
    (depend graph (car dep) (cdr dep))))

(define (depends? dg from to)
  (member to (transitive-dependencies dg from)))

(define (depend dg from to)
  (when (depends? dg to from)
    (raise-user-error 'depend (format "cycle detected between ~a and ~a" from to)))

  (graph-add-edge dg from to))

(define direct-dependencies graph-successors)

(define (transitive-dependencies dg from)
  (let loop ([transitive-deps (set)]
             [deps (direct-dependencies dg from)])
    (match deps
      [(list)
       (set->list transitive-deps)]

      [(list dep deps ...)
       (loop (set-add transitive-deps dep)
             (for/fold ([deps deps])
                       ([direct-dep (direct-dependencies dg dep)]
                        #:when (not (set-member? transitive-deps direct-dep)))
               (cons direct-dep deps)))])))

(define starting-order (compose reverse graph-toposort))
(define stopping-order graph-toposort)

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
