#lang racket

(require rackunit
         rackunit/text-ui
         "lexer.rkt")



(define lexer-tests
  (test-suite
   "Pruebas para lexer.rkt"
   (test-case "characters"
              (check-true (equal? (stream->list(lex-from-string "(")) (list (token 'open-paren #f 1 0))))
              (check-true (equal? (stream->list(lex-from-string ")")) (list (token 'close-paren #f 1 0))))
              (check-true (equal? (stream->list(lex-from-string "+")) (list (token 'binop '+ 1 0))))
              (check-true (equal? (stream->list(lex-from-string "*")) (list (token 'binop '* 1 0))))
              (check-true (equal? (stream->list(lex-from-string " ")) '()))
              (check-true (equal? (stream->list(lex-from-string "define")) (list (token 'define #f 1 0))))
              (check-true (equal? (stream->list(lex-from-string "x23")) (list (token 'identifier 'x23 1 0))))
              (check-true (equal? (stream->list(lex-from-string "1234")) (list (token 'number 1234 1 0))))
              )
   (test-case "examples"
              (check-true (equal? (stream->list(lex-from-string "(define y 100)
(define x 200) (+ x y)")) (list
                           (token 'open-paren #f 1 0)
                           (token 'define #f 1 1)
                           (token 'identifier 'y 1 8)
                           (token 'number 100 1 10)
                           (token 'close-paren #f 1 13)
                           (token 'open-paren #f 2 0)
                           (token 'define #f 2 1)
                           (token 'identifier 'x 2 8)
                           (token 'number 200 2 10)
                           (token 'close-paren #f 2 13)
                           (token 'open-paren #f 2 15)
                           (token 'binop '+ 2 16)
                           (token 'identifier 'x 2 18)
                           (token 'identifier 'y 2 20)
                           (token 'close-paren #f 2 21))))
              )))

(run-tests lexer-tests)
