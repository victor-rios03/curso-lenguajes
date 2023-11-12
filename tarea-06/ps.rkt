#lang racket
(require pict)
(require racket/draw)

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
(explode "hola")
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
(bundle '("a" "b" "c" "d") 1000)

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
  (char<? a b))

(isort (list #\e #\d #\a #\b) char-compare)




;;;Problema 10
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
             (filter (lambda (x) (equal? x pivot)) ls)
             (quicksort-any (largers-any ls pivot compare) compare)
             )]
    ))


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
    [(equal? (first ls) pivot) (largers-any (rest ls) pivot compare)]
    [else
     (cons (first ls) (largers-any (rest ls) pivot compare))]))


(quicksort-any (list #\e #\d #\a #\b) char-compare)
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

(define (bundle-check s n)
  (cond
    [(null? s) null]
    [(equal? n 0) (error 'bundle-check "Error")]
    [else
     (cons (implode (take s n))
           (bundle-check (drop s n) n))]))
;(bundle-check '("a" "b" "c" "d") 0)
;Problema 17
;;; El error seria que el menos igual no toma en cuenta la repeticion del pivote en la lista
(define (smallers-error l n)
  (cond
    [(empty? l) '()]
    [else (if (<= (first l) n)
              (cons (first l) (smallers-error (rest l) n))
              (smallers-error (rest l) n))]))


;Problema 18
;;; En los casos en los que m sea mayor que 0 o m y n sean mayor que 0

;Problema 19
;;; La funcion find-largest-divisor es una funcion recursiva estructural usada para encontrar el comun divisor mas largo entre dos numeros n y m.
;;; Caso base: Si k es igal entonces la funcion devuelve 1.
;;; Si el cociente de ambos numeros es 0, entonces k es un comun divisor. Si esto es verdad devuleve k
;;; Si las condiciones no se cumple hace una llamada recursiva con k menos 1.
;;; La llamada en la funcion gcd-structural  con el minimo de n y m como el valor inicial de k, ya que el comun divisor no puede ser mayor que el menor de ambos numeros
(define (gcd-structural n m)
  (define (find-largest-divisor k)
    (cond [(= k 1) 1]
          [(= (remainder n k) (remainder m k) 0) k]
          [else (find-largest-divisor (- k 1))]))
  (find-largest-divisor (min n m)))

;;; Problema 20
;;; La funcion find-largest-divisor es una funcion recursiva generativa usada para encontrar el comun divisor mas largo entre dos numeros n y m.
;;; Caso base: Si min es igual a 0 devuelve max que significa que el maximo comun divisor se encontro.
;;; Si el caso base no se cumple se hace una llamada recursiva con el minimo y el residuo de max hy min.
;;; La llamada en la funcion gcd-generative con el maximo de n y m, y el minimo de n y m para asegurarse que el minimo y el maximo sean usados.
(define (gcd-generative n m)
  (define (find-largest-divisor max min)
    (if (= min 0)
        max
        (find-largest-divisor min (remainder max min))))
  (find-largest-divisor (max n m) (min n m)))

;;; Problema 21

;;; Pequeños
(time (gcd-structural 100 99))
(time (gcd-generative 100 99))

;;; Medianos
(time (gcd-structural 104729 104728))
(time (gcd-generative 104729 104728))
;;; Grandes
(time (gcd-structural 10523145 10523144))
(time (gcd-generative 10523145 10523144))

;;; Problema 22
;;; Si estas trabajando con entradas pequeñas y prefieres tener un código fácil de leer puede llegar a convenir usar el que es menos eficiente.

;;; Problema 23

(define (triangle side width color)
  (define w side)
  (define h (* side (sin (/ pi 3))))
  (define (draw-it ctx dx dy)
    (define prev-pen (send ctx get-pen))
    (define path (new dc-path%))
    (send ctx set-pen (new pen% [width width] [color color]))
    (send path move-to 0 h)
    (send path line-to w h)
    (send path line-to (/ w 2) 0)
    (send path close)
    (send ctx draw-path path dx dy)
    (send ctx set-pen prev-pen))
  (dc draw-it w h))

(define (sierpinski side)
  (cond [(<= side 4) (triangle side 1 "red")]
        [else
         (define div (sierpinski (/ side 2)))
         (vc-append div (hc-append div div))]))

(provide bundle
         explode)
