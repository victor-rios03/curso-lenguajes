#lang racket

;Define y prueba un procedimiento countdown que toma un natural y regresa una lista de los naturales menores o
;iguales a ese numero, en orden descendente:

(define (countdown x) 
    (cond [(< x 0) null] 
          [else (cons x (countdown (sub1 x)))]))
(countdown 5)

;Define un procedimiento insertL que tome dos símbolos y una lista y regrese una nueva lista con el segundo símbolo insertado
;antes de cada aparición del primer símbolo, utiliza únicamente eqv? para comparar.

(define (insertL a b lst)
  (cond [(null? lst) null]
        [(eq? a (first lst))
         (cons b (cons a (insertL a b (rest lst))))]
        [else
         (cons (first lst) (insertL a b (rest lst)))]))

(insertL 'x 'y '(x z z x y x))

;Define un procedimiento remv-1st que toma un símbolo y una lista y regresa una nueva lista con la primera aparición
;del símbolo eliminada.

(define (remv-1st x lst)
  (cond [(null? lst) null]
        [(eq? x (first lst)) (rest lst)]
        [else (cons (first lst) (remv-1st x (rest lst)))]))

(remv-1st 'x '(x y z x))

;Define un procedimiento map que toma un procedimiento p de un argumento y una lista ls y regresa una nueva lista que
;contiene los resultados de aplicar p a los elementos de ls.

(define (map p ls)
  (if (null? ls)
      null
      (cons (p (first ls))
            (map p (rest ls)))))

;Define un procedimiento filter que toma un predicado y una lista y regresa una nueva lista que contiene los
;elementos que satisfacen el predicado.

(define (filter p ls)
  (cond [(null? ls) null]
        [(p (first ls))
         (cons (first ls) (filter p (rest ls)))]
        [else
         (filter p (rest ls))]))

;Define un procedimiento zip que toma dos listas y forma una nueva lista, cada elemento en esta es un par formado de
;la combinación de los elementos correspondientes a las dos listas de entrada. Si las dos listas no tienen la misma longitud,
;ignora la cola de la mas larga.

(define (zip ls1 ls2)
  (cond [(null? ls1) null]
        [(null? ls2) null]
        [else (cons (cons (first ls1) (first ls2)) (zip (rest ls1) (rest ls2)))]))

;Define un procedimiento list-index-ofv que toma un elemento y una lista y regresa el índice de ese elemento en
;la lista (base 0).

(define (list-index-ofv x ls)
  (define (next-step x ls i)
    (cond [(null? ls) -1]
          [(equal? x (first ls)) i]
          [else (next-step x (rest ls) (add1 i))]))
  (next-step x ls 0))

;Define un procedimiento append que toma dos listas, ls1 y ls2 y que concatena ls1 a ls2.

(define (append ls1 ls2)
  (if (null? ls1)
      ls2
      (cons (first ls1)
            (append (rest ls1) ls2))))

;Define un procedimiento reverse que toma una lista y regresa una lista con los mismos elementos en orden inverso.

(define (reverse ls)
  (define (next-step ls rv)
    (if (null? ls)
        rv
        (next-step (rest ls) (cons (first ls) rv))))
  (next-step ls null))

;Define un procedimiento repeat que toma una lista y un natural y regresa una nueva lista con secuencias repetidas de la
;lista de entrada, donde la cantidad de repeticiones es igual al natural dado.

(define (repeat ls n)
  (define (recur ls* n)
    (cond [(zero? n) null]
          [(null? ls*) (repeat ls (sub1 n))]
          [else (cons (first ls*) (recur (rest ls*) n))]))
  (recur ls n))

;Define un procedimiento same-lists* que toma dos listas (cuyos elementos posiblemente son a su vez listas) y regresa #t si son iguales y #f de lo contrario.

(define (same-lists* ls1 ls2)
  (cond [(null? ls1) (null? ls2)]
        [(pair? ls1)
         (and (pair? ls2)
              (same-lists* (first ls1) (first ls2))
              (same-lists* (rest ls1) (rest ls2)))]
        [else
         (eqv? ls1 ls2)]))


;Las expresiones (a b) y (a . (b . ())) son equivalentes. Sabiendo esto, reescribe la expresión ((w x) y (z))
;usando tantos puntos como sea posible. Asegúrate de probar tu solución usando el predicado equal?

;Define un procedimiento binary->natural que toma una lista de ceros y unos representando un numero binario sin signo
;en orden inverso y que regrese ese numero.

(define (binary->natural ls)
  (if (null? ls) 0
      (+ (first ls)
         (* 2 (binary->natural (rest ls))))))

;Define la división usando recursividad. Tu función de división div debe
;solo funcionar cuando el segundo número divide por completo al primero.

(define (div n m)
  (if (equal? n 0) 0
      (add1 (div (- n m) m))))

;Define una función recursiva append-map similar a map pero que concatena
;los resultados de aplicar el primer argumento a cada elemento del segundo.

(define (append-map p ls)
  (if (null? ls)
      null
      (append (p (first ls))
              (append-map p (rest ls)))))

;Define una función set-difference que toma dos listas sin elementos
;repetidos s1 y s2 y regresa una lista con todos los elementos de s1 que no son elementos de s2.

(define (set-difference s1 s2)
  (cond [(null? s1) null]
        [(member (first s1) s2)
         (set-difference (rest s1) s2)]
        [else
         (cons (first s1) (set-difference (rest s1) s2))]))

(set-difference '(1 2 3 4 5) '(2 6 4 8))

;Define una función foldr que toma tres argumentos: una función binaria, un acumulador inicial y una lista.
;Esta función recorre la lista de derecha a izquierda y en cada elemento invoca la función con el elemento de la lista
;y el valor del acumulador, luego actualiza el acumulador con el resultado de esta invocación.
;El resultado de esta función es el acumulador final después de recorrer toda la lista.

(define (foldr f a ls)
  (if (null? ls) a
      (f (first ls) (foldr f a (rest ls)))))

;El producto cartesiano es definido sobre una lista de conjuntos (representados como listas sin duplicados).
;El resultado es una lista de tuplas (representadas como listas). Cada tupla tiene en la primera posición un elemento
;del primer conjunto, en la segunda posición un elemento del segundo conjunto, etc. La lista resultante debe contenter
;todas las combinaciones. El orden en la lista resultante no es relevante.

(define (cartesanian-product sets)
  (define (extend-tups x tups)
    (map (lambda (tup) (cons x tup)) tups))
  (define (extend-every set tuples)
    (append-map (lambda (x) (extend-tups x tuples)) set))
  (foldr extend-every '(()) sets))

;Reescribe las siguientes funciones pero utilizando foldr:

(define (filter-fr p ls)
  (define (filter x acc)
    (if (p x)
        (cons x acc)
        acc))
  (foldr filter null ls))

(define (map-fr p ls)
  (define (map x acc)
    (cons (p x) acc))
  (foldr map null ls))

(define (append-fr ls1 ls2)
  (foldr cons ls2 ls1))

(define (reverse-fr ls)
  (define (reverse x acc)
    (append acc (list x)))
  (foldr reverse null ls))

(define (binary->natural-fr bits)
  (define (binary->natural x acc)
    (+ x (* 2 acc)))
  (foldr binary->natural 0 bits))

(define (append-map-fr p ls)
  (define (append-map x acc)
    (append (p x) acc))
  (foldr append-map null ls))

(define (set-difference-fr set1 set2)
  (define (set-difference x acc)
    (if (member x set2)
        acc
        (cons x acc)))
  (foldr set-difference null set1))




              
