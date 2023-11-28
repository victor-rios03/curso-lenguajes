#lang racket/base

(require racket/contract
         syntax/readerr
         "let-locs.rkt")

(define (read-error message beg end)
  (raise-read-error
   message
   (or (file-path) 'interactive)
   (and beg (pos-line beg))
   (and beg (pos-col beg))
   (and beg (pos-offset beg))
   (and beg end (- (pos-offset end) (pos-offset beg)))))

(provide
 (contract-out
  [read-error (-> string? (or/c #f pos?) (or/c #f pos?)
                  any)]))
