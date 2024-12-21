#lang racket/base

(require component)

(provide
 prod-system)

(define-system prod
  [app (broker db) void]
  [broker (db foo) void]
  [db void])
