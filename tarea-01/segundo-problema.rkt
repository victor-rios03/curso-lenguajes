#lang stacker/smol/state
;#:no-trace

(deffun (pause) 0)
(defvar v1 (mvec 0))
(defvar v2 (mvec 1))

(vec-set! v1 0 v2)
(vec-set! v2 0 v1)
(pause)
