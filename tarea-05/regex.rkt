#lang racket

(define open-paren-regex #rx"\\(")
(define close-paren-regex #rx"\\)")
(define define-regex #rx"define")
(define sum-regex #rx"\\+")
(define mult-regex #rx"\\*")
(define identifier-regex #rx"[xyz][xyz0-9]*")
(define number-regex #rx"[\\+\\-]?[0-9]+")

(provide open-paren-regex
         close-paren-regex
         define-regex
         sum-regex
         mult-regex
         identifier-regex
         number-regex)