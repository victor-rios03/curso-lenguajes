#lang racket

;Problema 1
;;; unit-string? : string? -> bool
;;; Verifica si una cadena es unitaria
(define (unit-string? x)
  (and (string? x)
       (= (string-length x) 1)))

;;; unit-string-list? : string? -> bool
;;; Verifica si una lista tiene cadenas unitarias
(define (unit-string-list? x)
  (or (null? x)
      (and (pair? x)
           (string? (car x))
           (=  (string-length (car x)) 1)
           (unit-string-list? (cdr x)))))

;;; explode : string? -> string
;;; Convierte una cadena en una lista de cadenas unitarias
(define (explode s)
  (unless (string? s)
    (error 'explode "esperaba una cadena, pero recibi: ~e" s))
  (map string (string->list s)))

;;; implode : list? -> string
;;; Convierte una lista de cadenas unitarias en una cadena
(define (implode ls)
  (unless (unit-string-list? ls)
    (error 'implode "esperaba una lista de cadenas unitarias, pero recibi: ~e" ls))
  (apply string-append ls))
;Problema 2

;;; take : list? int? -> list
;;; Elimina n elementos de enfrente de la lista
(define (take l n)
  (cond
    ((<= n 0) '())               
    ((null? l) '())            
    (else (cons (car l)        
                (take (cdr l) (- n 1))))))

;;; drop : list? int? -> list
;;; Elimina n elemenos de inicio de la lista
(define (drop l n)
  (cond
    ((<= n 0) l)               
    ((null? l) '())            
    (else (drop (cdr l) (- n 1)))))

(define (bundle s n)
  (cond
    [(null? s) null]
    [else
     (cons (implode (take s n))
           (bundle (drop s n) n))]))

;;; list->chunks : list? int? list
;;; Divide una lista en listas de n tamanio
(define (list->chunks l n)
  (if (or (<= n 0) (empty? l))
      '()
      (cons (take l n)
            (list->chunks (drop l n) n))))

;;; bundle-chunks : string
(define (bundle-chunks s n)
  (cond
    [(null? s) null]
    [else
     (cons (implode (first s))
           (bundle-chunks (rest s) n))]))

;;; partition : string? int? -> string
(define (partition s n)
  (if (or (<= n 0) (equal? "" s))
      '()
      (cons (substring s 0 n)
            (partition (substring s n) n))))

;Problema 7
;;; isort : list? function? -> list
(define (isort ls compare)
  (if (empty? ls)
      null
      (insert (first ls)
              (isort (rest ls) compare)
              compare)))

;;; insert : int? list? function? -> list
(define (insert n ls compare)
  (cond
    [(empty? ls) (list n)]
    [(compare n (first ls)) (cons n ls)]
    [else (cons (first ls) (insert n (rest ls) compare))]))

;;; char-compare : char? char? -> bool
(define (char-compare a b)
  (char<=? a b))

;(isort (list #\e #\d #\a #\b) char-compare)

;;; quicksort : list? -> list
(define (quicksort ls)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
     (append (quicksort (smallers ls pivot))
             (filter (lambda (x) (= x pivot)) ls) 
             (quicksort (largers ls pivot)))]))

(define (smallers ls pivot)
  (cond
    [(empty? ls) null]
    [(< (first ls) pivot) (cons (first ls) (smallers (rest ls) pivot))]
    [else
     (smallers (rest ls) pivot)]))

(define (largers ls pivot)
  (cond
    [(empty? ls) null]
    [(> (first ls) pivot) (cons (first ls) (largers (rest ls) pivot))]
    [else
     (largers (rest ls) pivot)]))

;(quicksort '(1 2 1 1 2 3 2))

;Problema 11---------------------------------
(define (quicksort-any ls compare)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
     (append (quicksort-any (smallers-any ls pivot compare) compare)
             (filter (lambda (x) (= x pivot)) ls) 
             (quicksort-any (largers-any ls pivot compare) compare))]))

(define (smallers-any ls pivot compare)
  (cond
    [(empty? ls) null]
    [(compare (first ls) pivot) (cons (first ls) (smallers-any (rest ls) pivot compare))]
    [else
     (smallers-any (rest ls) pivot compare)]))

(define (largers-any ls pivot compare)
  (cond
    [(empty? ls) null]
    [(compare (first ls) pivot) (largers-any (rest ls) pivot compare)]
    [else
     (cons (first ls) (largers-any (rest ls) pivot compare))]))

(define (comparison x y)
  (> x y))

;(quicksort-any '(1 2) comparison)
;Problema 12---------------------------------

;(time (quicksort '(1 3 2 4 19 4 2 42 -42 451 412 32131 4103 4312 1323 41231 2323 342)))

;Problema 13--------------------------------- Utiliza filters para definir smallers y largers

(define (smallersf ls pivot)
  (filter (lambda (x) (< x pivot)) ls))

(define (largersf ls pivot)
  (filter (lambda (x) (> x pivot)) ls))

;(largersf '(1 2 4 5) 2)

;Problema 14-------------------------------- Implementa smallers y largers como procedimientos locales internos en quicksort

(define (quicksort-local ls)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
     (append (quicksort-local (filter (lambda (x) (< x pivot)) ls))
             (filter (lambda (x) (= x pivot)) ls) 
             (quicksort-local (filter (lambda (x) (> x pivot)) ls)))]))

;Problema 16
(provide bundle
         explode)
