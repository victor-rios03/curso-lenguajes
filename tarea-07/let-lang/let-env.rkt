#lang racket/base

(require racket/contract
         racket/match
         "let-vals.rkt")

(struct environment (binding parent) #:transparent)

(define (init-env)
  (environment #f #f))

(define (apply-env env var)
  (match env
    [(environment #f _)
     (error (format "Unbound identifier ~a" var))]
    [(environment (cons (== var) val) _)
     val]
    [(environment (cons _ val) parent)
     (apply-env parent var)]))

(define (extend-env var val parent)
  (environment (cons var val) parent))

(provide
 environment?
 (contract-out
  [init-env (-> environment?)]
  [apply-env (-> environment? symbol?
                 expval?)]
  [extend-env (-> symbol? expval? environment?
                  environment?)]))
