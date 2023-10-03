#lang racket

(require rackunit
         rackunit/text-ui
         "regex.rkt")

;(regexp-match* open-paren-regex "(define pi(+3 (* 2 -12) x2))")
;(regexp-match* close-paren-regex "(define pi (+3 (* 2 -12) x2))")
;(regexp-match* sum-regex "(define pi (+3 (* 2 -12) x2))")
;(regexp-match* mult-regex "(define pi (+3 (* 2 -12) x2))")
;(regexp-match* identifier-regex "(define pi (+3 (* 2 -12) x2))")
;(regexp-match* number-regex "(define pi (3 (* 2 -12) x2))")

(define regex-tests
  (test-suite
   "Pruebas para regex.rkt"
   (test-case "open-paren-regex"
              (check-true (regexp-match? open-paren-regex "(define pi (+3 (* 2 -12) x2))")))
   (test-case "close-paren-regex"
              (check-true (regexp-match? close-paren-regex "(define pi (+3 (* 2 -12) x2))")))
   (test-case "define-regex"
              (check-true (regexp-match? define-regex "(define pi (+3 (* 2 -12) x2))")))
   (test-case "sum-regex"
              (check-true (regexp-match? sum-regex "(define pi (+3 (* 2 -12) x2))")))
   (test-case "mult-regex"
              (check-true (regexp-match? mult-regex "(define pi (+3 (* 2 -12) x2))")))
   (test-case "identifier-regex"
              (check-true (regexp-match? identifier-regex "(define pi (+3 (* 2 -12) x2))")))
   (test-case "number-regex"
              (check-true (regexp-match? number-regex "(define pi (+ 3 (* 2 -12) x2))")))))

(run-tests regex-tests)
