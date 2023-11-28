#lang racket

(require "let-locs.rkt"
         "let-tokens.rkt"
         "let-ast.rkt"
         "let-lexer.rkt"
         "let-parser.rkt"
         "let-vals.rkt"
         "let-eval.rkt")

(define interpret
  (compose value-of-program
           parse-let
           lex-let))

(define interpret-string
  (compose interpret open-input-string))

(port-count-lines-enabled #t)

(define (interpret-file path)
  (parameterize ([file-path path])
    (call-with-input-file path interpret)))

(provide
 interpret
 interpret-string
 interpret-file)
