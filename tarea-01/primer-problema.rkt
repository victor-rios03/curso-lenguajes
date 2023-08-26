#lang stacker/smol/fun
;#:no-trace

(deffun (pause) 0)

(deffun (g)
  (defvar y 6)
  (+ (pause) y))

(deffun (f)
  (defvar x 3)
  (+ (g) x))

(f)