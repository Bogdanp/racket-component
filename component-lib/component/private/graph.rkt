#lang racket/base

;; Provides a simple directed graph data structure that can sort its
;; vertices topographically.

(require racket/contract/base
         racket/match
         racket/set)

(provide (struct-out exn:fail:graph)
         (struct-out exn:fail:graph:cycle)

         (contract-out
          [struct graph ((mappings hash?))]
          [make-graph (->* () ((listof pair?)) graph?)]
          [graph-add-edge (-> graph? any/c any/c graph?)]
          [graph-remove-edge (-> graph? any/c any/c graph?)]
          [graph-edges (-> graph? (listof pair?))]
          [graph-successors (-> graph any/c (listof any/c))]
          [graph-toposort (-> graph? (listof any/c))]))

(struct exn:fail:graph exn:fail ())
(struct exn:fail:graph:cycle exn:fail:graph ())

(struct graph (mappings)
  #:transparent)

(define (make-graph [edges '()])
  (for/fold ([g (graph (hasheq))])
            ([edge edges])
    (graph-add-edge g (car edge) (cdr edge))))

(define (graph-add-edge g from to)
  (define mappings (graph-mappings g))
  (define successors (hash-ref mappings from '()))
  (graph (hash-set mappings from (or (member to successors)
                                     (cons to successors)))))

(define (graph-remove-edge g from to)
  (define mappings (graph-mappings g))
  (define successors (remove to (hash-ref mappings from '())))
  (graph (hash-set mappings from successors)))

(define (graph-successors g vertex)
  ;; Preserve insertion order when returning the successors.
  (reverse (hash-ref (graph-mappings g) vertex '())))

(define (graph-edges g)
  (for*/list ([(from successors) (graph-mappings g)]
              [to successors])
    (cons from to)))

; top o' the sortin' to ya, dear reader!
(define (graph-toposort g)
  (define (go free-vertices g sorted)
    (match free-vertices
      [(list)
       (values g (reverse sorted))]

      [(list free-vertex free-vertices ...)
       (define-values (g* free-vertices*)
         (for/fold ([g g] [free-vertices free-vertices])
                   ([pointed-vertex (graph-successors g free-vertex)])
           (define g* (graph-remove-edge g free-vertex pointed-vertex))
           (if (free-vertex? g* pointed-vertex)
               (values g* (cons pointed-vertex free-vertices))
               (values g* free-vertices))))

       (go free-vertices* g* (cons free-vertex sorted))]))

  (define-values (g* vertices)
    (go (free-vertices g) g '()))

  (when (not (null? (graph-edges g*)))
    (raise (exn:fail:graph:cycle "cycle detected" (current-continuation-marks))))

  vertices)

(define (free-vertices g)
  (define-values (pointers pointees)
    (for/fold ([pointers (set)]
               [pointees (set)])
              ([edge (graph-edges g)])
      (values (set-add pointers (car edge))
              (set-add pointees (cdr edge)))))
  (set->list (set-subtract pointers pointees)))

(define (free-vertex? g vertex)
  (andmap (lambda (edge)
            (not (eq? (cdr edge) vertex)))
          (graph-edges g)))
