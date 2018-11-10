#lang racket/base

(require racket/contract/base
         racket/match
         racket/set)

(provide (struct-out exn:fail:graph)
         (struct-out exn:fail:graph:cycle)

         (contract-out
          [struct graph ((mappings hash?))]
          [make-graph (->* () ((listof edge/c)) graph?)]
          [graph-add-edge (-> graph? edge/c graph?)]
          [graph-remove-edge (-> graph? edge/c graph?)]
          [graph-edges (-> graph? (listof edge/c))]
          [graph-ref (-> graph any/c (listof any/c))]
          [graph-toposort (-> graph? (listof any/c))]))

(define edge/c
  (cons/c any/c any/c))

(struct exn:fail:graph exn:fail ())
(struct exn:fail:graph:cycle exn:fail:graph ())

(struct graph (mappings)
  #:transparent)

(define (make-graph [edges '()])
  (for/fold ([g (graph (hasheq))])
            ([edge edges])
    (graph-add-edge g edge)))

(define (graph-add-edge g edge)
  (define from-vertex (car edge))
  (define to-vertex (cdr edge))
  (define mappings (graph-mappings g))
  (define successors (hash-ref mappings from-vertex '()))
  (graph (hash-set mappings from-vertex (or (member to-vertex successors)
                                            (cons to-vertex successors)))))

(define (graph-remove-edge g edge)
  (define from-vertex (car edge))
  (define to-vertex (cdr edge))
  (define mappings (graph-mappings g))
  (define successors (remove to-vertex (hash-ref mappings from-vertex '())))
  (graph (hash-set mappings from-vertex successors)))

(define (graph-ref g vertex)
  (hash-ref (graph-mappings g) vertex '()))

(define (graph-edges g)
  (for*/list ([(vertex successors) (graph-mappings g)]
              [other-vertex successors])
    (cons vertex other-vertex)))

(define (graph-free-vertices g)
  (define-values (pointers pointees)
    (for/fold ([pointers (set)]
               [pointees (set)])
              ([edge (graph-edges g)])
      (values (set-add pointers (car edge))
              (set-add pointees (cdr edge)))))
  (set->list (set-subtract pointers pointees)))

(define (graph-free-vertex? g vertex)
  (andmap (lambda (edge)
            (not (eq? (cdr edge) vertex)))
          (graph-edges g)))

(define (graph-toposort g)
  (define (go free-vertices g sorted)
    (match free-vertices
      [(list)
       (values g (reverse sorted))]

      [(list free-vertex free-vertices ...)
       (define-values (g* free-vertices*)
         (for/fold ([g g] [free-vertices free-vertices])
                   ([pointed-vertex (graph-ref g free-vertex)])
           (define g* (graph-remove-edge g (cons free-vertex pointed-vertex)))
           (if (graph-free-vertex? g* pointed-vertex)
               (values g* (cons pointed-vertex free-vertices))
               (values g* free-vertices))))

       (go free-vertices* g* (cons free-vertex sorted))]))

  (define-values (g* vertices)
    (go (graph-free-vertices g) g '()))

  (when (not (null? (graph-edges g*)))
    (raise (exn:fail:graph:cycle "cycle detected" (current-continuation-marks))))

  vertices)

(module+ test
  (require rackunit)

  (define (symbol<=? x . xs)
    (or (apply eq? x xs)
        (apply symbol<? x xs)))

  (define (pair<=? a-p b-p)
    (and (symbol<=? (car a-p) (car b-p))
         (symbol<=? (cdr a-p) (cdr b-p))))

  (check-equal?
   (sort (graph-edges (make-graph '((a . b) (a . c) (b . c)))) pair<=?)
   '((a . b)
     (a . c)
     (b . c)))

  (check-equal?
   (graph-edges
    (graph-remove-edge
     (make-graph '((a . b)
                   (a . c)))
     '(a . c)))
   '((a . b)))

  (check-equal?
   (graph-toposort (make-graph '()))
   '())

  (check-equal?
   (graph-toposort (make-graph '((a . b))))
   '(a b))

  (check-equal?
   (graph-toposort (make-graph '((a . b) (c . d))))
   '(c d a b))

  (check-equal?
   (graph-toposort (make-graph '((a . b) (a . c) (b . d) (d . e))))
   '(a b d e c))

  (check-equal?
   (graph-toposort (make-graph '((a . b) (a . c) (a . d) (b . c) (b . d) (c . d))))
   '(a b c d))

  (check-exn
   exn:fail:graph:cycle?
   (lambda ()
     (graph-toposort (make-graph '((a . b) (b . a)))))))
