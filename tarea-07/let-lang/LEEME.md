# Lenguaje LET

En este directorio se presenta la implmentación de un intérprete del
lenguaje LET.

Identificamos las etapas en que la información es transformada:
1. Flujo de caracteres
2. Flujo de tokens
3. Árbol de sintaxis abstracta
4. Valor del programa

Identificamos las componentes del sistema:
a. Analizador léxico (lexer)
b. Analizador sintáctico (parser)
c. Evaluador (eval)

## Especificación del lenguaje

### Especificación léxica

La implementación del lexer se encuentra en `let-lexer.rkt`.

- Un commentario consiste desde el caracter `#` hasta el salto de
  linea o fin de archivo.
- Un entero es una secuencia no vacía de dígitos, opcionalmente
  prefijados con `+` o `-`.
- Un identificador es una secuencia no vacía de símbolos alfabéticos y
  dígitos, obligatoriamente iniciando con un símbolo alfabético.
- Se reconocen los lexemas:
  - Paréntesis abierto `(`
  - Paréntesis cerrado `)`
  - Operador `zero?`
  - Operador `-`
  - Operador `,`
  - Palabra reservada `if`
  - Palabra reservada `then`
  - Palabra reservada `else`
  - Palabra reservada `let`
  - Operador `=`
  - Palabra reservada `in`
  
El lexer toma un flujo de caracteres, en particular un `input-port?`,
y regresa un flujo de tokens, en particular un `stream?` donde cada
elemento es un objeto `pos-token?`. La estructura de los tokens se
encuentra en `let-tokens.rkt`. Un `pos-token` es especial porque
contiene a tokens de otros tipos y la posición de inicio y fin del
codigo fuente analizado.

### Especificación sintáctica

La implementación del parser se encuentra en `let-parser.rkt`.

La sintaxis concreta y abstracta corresponden se especifican en el
documento `let-spec.pdf`.

El parser toma un flujo de tokens y construye un árbol de sintaxis
abstracta (AST). La estructura de los AST se encuentra en
`let-ast.rkt`.

### Especificación semántica

La implementación del eval se encuentra en `let-eval.rkt` y depende de
las estructuras de los valores expresados, implementadas en
`let-vals.rkt`.

Las reglas semánticas se especifican en el documento `let-spec.pdf`.

El evaluador toma un AST de programa y regresa un valor expresado.

### Intérprete

El intérprete del lenguaje se implementa en `let-interp.rkt`, utiliza
el lexer, parser y eval para realizar el proceso de interpretación
completo.

Se exponen procedimientos para interpretar código LET desde una cadena
de caracteres, un archivo o directamente de un flujo de entrada.

### REPL

Se incluye un Read-Eval-Print-Loop como interfaz al intérprete en el
archivo `let-repl.rkt`, al correr este archivo en Racket se presenta
una interfaz de linea de comandos que interpreta programas LET.

## Archivos

### Listado alfabético

- `example.let` programa LET de ejemplo
- `LEEME.md` este archivo
- `let-ast.rkt` estructura de los AST
- `let-env.rkt` estructura del entorno
- `let-errors.rkt` reporte de errores
- `let-eval.rkt` evaluador
- `let-interp.rkt` intérprete
- `let-lexer.rkt` lexer
- `let-lexer-test.rkt` pruebas para lexer
- `let-locs.rkt` estructura de posiciones en archivo
- `let-parser.rkt` parser
- `let-repl.rkt` REPL
- `let-tokens.rkt` estructuras de tokens
- `let-vals.rkt` estructura de valores expresados

### Por dependencias

- `let-repl.rkt`
  - `let-interp.rkt`
    - `let-lexer.rkt`
      - `let-errors.rkt`
      - `let-locs.rkt`
      - `let-tokens.rkt`
    - `let-parser.rkt`
      - `let-errors.rkt`
      - `let-locs.rkt`
      - `let-tokens.rkt`
      - `let-ast.rkt`
    - `let-eval.rkt`
      - `let-ast.rkt`
      - `let-vals.rkt`
      - `let-env.rkt`
