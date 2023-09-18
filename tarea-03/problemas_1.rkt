#lang stacker/smol/state
;#:no-trace

(deffun (pause) 0)

(defvar v1 (mvec 0))
(defvar v2 (mvec 1))
(defvar v0 (mvec v1 v2))

(vec-set! v1 0 v2)
(vec-set! v2 0 v1)
v0
(pause)

;Programa que define tres vectores. Dos con un numero y uno con la direccion de ambos vectores.
;Se cambia el contenido de ambos vectores con numeros y despues se imprime el vector que tiene como contenido sus direcciones.
