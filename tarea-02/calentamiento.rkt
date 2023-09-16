#lang racket

(define pi 3.14)
(define (area-circle r)
  (* pi r r))

(define (circle-properties r)
  (list (* pi r r) (* 2 pi r)))

(define (rectangle-properties lst)
  (define l (first lst))
  (define w (first (rest lst)))
  (list (*  l w) (+ l l w w)))

(define (find-needle lst)
    (cond [(equal? 'needle (first lst)) 0]
          [(equal? 'needle (second lst)) 1]
          [(equal? 'needle (third lst)) 2]
          [else -1]))

(define (abs x)
  (if (> x 0)
      x
      (- x)))

(define (inclis1 lst)
  (map add1 lst))

(define (even? num)
  (zero? (modulo num 2)))

(define (another-add n m ) 
    (cond
      [(zero? n) m]
      [else (add1 (another-add m (sub1 n)))]))
;(area-circle 5)

;(circle-properties 5)

;(rectangle-properties (list 2 4))

(find-needle (quote (hay needle hay)))

;(abs -2)

;(inclis1 '(1 2 3))
;map even? '(1 2 3))

;(another-add 4 0)



