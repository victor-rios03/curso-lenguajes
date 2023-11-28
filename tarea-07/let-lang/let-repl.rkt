#lang racket

(require "let-interp.rkt"
         "let-vals.rkt")

(define (get-line)
  (define ch (peek-char))
  (cond [(eof-object? ch)
         "exit"]
        [(char-whitespace? ch)
         (read-char)
         (get-line)]
        [else
         (read-line)]))

(define (repl)
  (printf "LET> ")
  (flush-output)
  (let ([str (string-trim (get-line))])
    (match str
      ["exit"
       (printf "bye bye\n")]
      ["quit"
       (printf "adios\n")]
      [_
       (with-handlers ([exn:fail:read?
                        (lambda (exn)
                          (printf "Error in ~a\n" (exn-message exn)))]
                       [exn:fail?
                        (lambda (exn)
                          (printf "Error: ~a\n" (exn-message exn)))])
         (match (interpret-string str)
           [(num-val x) (printf "~a\n" x)]
           [(bool-val x) (printf "~a\n" (if x "true" "false"))]
           [(null-val) (printf '())]
           [(pair-val x)
                      (cons (interpret-string first) (interpret-string rest))]))
         (repl)])))

(with-handlers ([exn:break?
                 (lambda args
                   (printf "User break, exiting...\n"))])
  (repl))
