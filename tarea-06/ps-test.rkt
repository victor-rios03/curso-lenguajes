#lang racket

(require rackunit
         rackunit/text-ui
         "ps.rkt")
; Problema 1
(define ps-tests
  (test-suite
   "Pruebas para ps.rkt"
   (test-case "igualdad"
              (check-equal? (bundle (explode "abcdefg") 3)
                            (list "abc" "def" "g"))

              (check-equal? (bundle '("a" "b") 3)
                            (list "ab"))

              (check-equal? (bundle '() 3)
                            '())
              (check-equal? (bundle '("a" "b" "c") 0)
                            '())
              (check-equal? (bundle (explode "abcdefg") 1)
                            (explode "abcdefg"))
              )))

(run-tests ps-tests)
