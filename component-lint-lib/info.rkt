#lang info

(define license 'BSD-3-Clause)
(define collection "component")
(define deps
  '("base"
    "review"))
(define review-exts
  '((component/review should-review-syntax? review-syntax)))
