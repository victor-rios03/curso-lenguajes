#lang racket
;numbers
;token-identifier
;
;35 y 188

(define debug? #f)

(define (read-char* input)
  (define ch (read-char input))
  (when debug?
    (printf "read char s from input%" ch))
  ch)

(define (peek-char* input)
  (define ch (peek-char input))
  (when debug?
    (printf "peeked char s from input%" ch))
  ch)

(define (char-digit? ch)
  (if (eof-object? ch)
      #f
      (char<=? #\0 ch #\9 )
      ))

(define (char-varletter? ch)
  (member ch '(#\x #\y #\z) char=?))

(define (char-delimiter? ch)
  (or (eof-object? ch)
      (char-whitespace? ch)
      (member ch '(#\( #\)))))

(define lex-path "<unknown>")
(define lex-line 0)
(define lex-col 0)

(struct token (type value line col)
  #:transparent)

(define (token-open-paren)
  (token 'open-paren #f lex-line lex-col))

(define (token-close-paren)
  (token 'close-paren #f lex-line lex-col))

(define (token-binop type)
  (token 'binop type lex-line lex-col))

(define (token-number value col)
  (token 'number value lex-line col))

(define (token-number/+ token)
  (token 'number
         (token-value token)
         (token-line token)
         (sub1 (token-col token))))

(define (token-number/- token)
  (token 'number
         (- (token-value token))
         (token-line token)
         (sub1 (token-col token))))

(define (token-identifier symbol col)
  (token 'identifier symbol lex-line col))

(define (token-define col)
  (token 'define #f lex-line col))

(define (lex-open-paren chars)
  (read-char* chars)
  (let ([token (token-open-paren)])
    (set! lex-col (add1 lex-col))
    (stream-cons token (lex chars))))

(define (lex-close-paren chars)
  (read-char* chars)
  (let ([token (token-close-paren)])
    (set! lex-col (add1 lex-col))
    (stream-cons token (lex chars))))

(define (lex-whitespace chars)
  (define ch (read-char* chars))
  (cond
    [(char=? ch #\newline)
     (set! lex-line (add1 lex-line))
     (set! lex-col 0)]
    [else
     (set! lex-col (add1 lex-col))])
  (lex chars))

(define (lex-sum-or-number chars)
  (read-char* chars)
  (let ([ch (peek-char* chars)])
    (cond
      [(char-digit? ch)
       (set! lex-col (add1 lex-col))
       (let* ([tokens (lex-plain-number chars)]
              [token (token-number/+ (stream-first tokens))])
         (stream-cons token (stream-rest tokens)))]
      [else
       (let ([token (token-binop '+)])
         (set! lex-col (add1 lex-col))
         (stream-cons token (lex chars)))])))

(define (lex-negative-number chars)
  (read-char* chars)
  (let ([ch (peek-char* chars)]
        [col lex-col])
    (cond
      [(char-digit? ch)
       (set! lex-col (add1 lex-col))
       (let* ([tokens (lex-plain-number chars)]
              [token (token-number/- (stream-first tokens))])
         (stream-cons token (stream-rest tokens)))]
      [else
       (signal-lex-error "minus operation not supported, expected a number"
                         lex-line col)])))

(define (lex-mult chars)
  (read-char* chars)
  (let ([ch (peek-char* chars)])
    (cond
      [(char-delimiter? ch)
       (let ([token (token-binop '*)])
         (set! lex-col (add1 lex-col))
         (stream-cons token (lex chars)))]
      [else
       (signal-lex-error "expected delimiter after multiplication" lex-line
                         lex-col)])))

(define (lex-identifier-or-keyword chars)
  (define (read-alphanumeric strport is-identifier?)
    (write (read-char* chars) strport)
    (set! lex-col (add1 lex-col))
    (let [(ch (peek-char* chars))]
      (cond [(char-delimiter? ch)
             is-identifier?]
            [(char-digit? ch)
             (read-alphanumeric strport is-identifier?)]
            [(char-varletter? ch)
             (read-alphanumeric strport is-identifier?)]
            [else
             (read-alphanumeric strport #f)])))
  (let* ([col lex-col]
         [strport (open-output-string)]
         [is-identifier? (read-alphanumeric strport (char-varletter? )
                                            (peek-char* chars))]
         [str (get-output-string strport)])
    (cond
      [is-identifier?
       (let ([token (token-identifier (string->symbol str) col)])
         (stream-cons token (lex chars)))]
      [(string=? str "define")
       (let ([token (token-define col)])
         (stream-cons token (lex chars)))]
      [else
       (signal-lex-error "expected define or identifier" lex-line col)])))

(define zero-char-val (char->integer #\0))

(define (lex-plain-number chars)
  (define (build-integer value)
    (define d (- (char->integer (read-char* chars)) zero-char-val))
    (set! lex-col (add1 lex-col))
    (let [(ch (peek-char* chars))]
      (cond [(char-delimiter? ch)
             (+ (* value 10) d)]
            [(char-digit? ch)
             (build-integer (+ (* value 10) d))]
            [else
             (signal-lex-error "expected digit" lex-line lex-col)])))
  (let* ([col lex-col]
         [value (build-integer 0)]
         [token (token-number value col)])
    (stream-cons token (lex chars))))

(define (lex-end-of-file chars)
  (close-input-port chars)
  empty-stream)

(define (signal-lex-error message line col)
  (error 'lex (format "lexical error at ~a:~a:~a: ~a" lex-path line col
                      message)))

(define (lex chars)
  (define ch (peek-char* chars))
  (cond
    [(eof-object? ch) (lex-end-of-file chars)]
    [(char=? ch #\() (lex-open-paren chars)]
    [(char=? ch #\)) (lex-close-paren chars)]
    [(char-whitespace? ch) (lex-whitespace chars)]
    [(char=? ch #\+) (lex-sum-or-number chars)]
    [(char=? ch #\-) (lex-negative-number chars)]
    [(char=? ch #\*) (lex-mult chars)]
    [(char-alphabetic? ch) (lex-identifier-or-keyword chars)]
    [(char-digit? ch) (lex-plain-number chars)]
    [else (signal-lex-error "unknown error" lex-line lex-col)]))

(define (lex-from-file path)
  (set! lex-path path)
  (set! lex-line 1)
  (set! lex-col 0)
  (lex (open-input-file path #:mode 'text)))

(define (lex-from-string str)
  (set! lex-path "<unknown>")
  (set! lex-line 1)
  (set! lex-col 0)
  (lex (open-input-string str)))
(provide lex-from-file
         lex-from-string
         char-digit?)
