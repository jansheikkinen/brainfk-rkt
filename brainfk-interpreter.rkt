#!/bin/racket
#lang racket

(define CELLMAX 255) (define NUMCELLS 255)
(struct operation (literal function) #:transparent)
(struct interpreter (tape tapeptr cells cellptr ops) #:transparent)
(define (@tape    interp) (interpreter-tape    interp))
(define (@tapeptr interp) (interpreter-tapeptr interp))
(define (@cells   interp) (interpreter-cells   interp))
(define (@cellptr interp) (interpreter-cellptr interp))
(define (@ops     interp) (interpreter-ops     interp))

(define (transform-cell proc lst index)
  (append (take lst index)
          (list* (proc (car (list-tail lst index)))
                 (cdr (list-tail lst index)))))

(define (incop interp token)
  (define (inc cells cellptr)
    (cond ((>= (list-ref cells cellptr) CELLMAX)
           (transform-cell (lambda (_) 0) cells cellptr))
          (else (transform-cell add1 cells cellptr))))

  (interpreter (@tape interp) (+ (@tapeptr interp) 1)
    (inc (@cells interp)(@cellptr interp)) (@cellptr interp) (@ops interp)))

(define (decop interp token)
  (define (dec cells cellptr)
    (cond ((<= (list-ref cells cellptr) 0)
           (transform-cell (lambda (_) CELLMAX) cells cellptr))
          (else (transform-cell sub1 cells cellptr))))

  (interpreter (@tape interp) (+ (@tapeptr interp) 1)
    (dec (@cells interp) (@cellptr interp)) (@cellptr interp) (@ops interp)))

(define (leftop interp token)
  (define (left cellptr)
    (cond ((<= cellptr 0) (- NUMCELLS 1))
          (else (sub1 cellptr))))

  (interpreter (@tape interp) (+ (@tapeptr interp) 1)
    (@cells interp) (left (@cellptr interp)) (@ops interp)))

(define (rightop interp token)
  (define (right cellptr)
    (cond ((>= cellptr (- NUMCELLS 1)) 0)
          (else (add1 cellptr))))

  (interpreter (@tape interp) (+ (@tapeptr interp) 1)
    (@cells interp) (right (@cellptr interp)) (@ops interp)))

(define (readop interp token)
  (define (read_ cells cellptr)
    (transform-cell (lambda (_) (char->integer (read-char))) cells cellptr))

  (interpreter (@tape interp) (+ (@tapeptr interp) 1)
    (read_ (@cells interp) (@cellptr interp)) (@cellptr interp) (@ops interp)))

(define (printop interp token)
  (display (integer->char (list-ref (@cells interp)
                                    (@cellptr interp))))
  (interpreter (@tape interp) (+ (@tapeptr interp) 1)
    (@cells interp) (@cellptr interp) (@ops interp)))

(define (loop interp token)
  (define starttoken #\[) (define endtoken #\])

  (define (startop interp)
    (define (match-token tape tapeptr depth)
      (unless (< tapeptr (length tape)) (error "Unmatched '[' token"))
      (cond ((eq? (list-ref tape tapeptr) starttoken)
             (match-token tape (+ tapeptr 1) (+ depth 1)))
            ((eq? (list-ref tape tapeptr) endtoken)
             (if (= 0 depth) (+ tapeptr 1)
               (match-token tape (+ tapeptr 1) (- depth 1))))
            (else (match-token tape (+ tapeptr 1) depth))))

    (if (eq? 0 (list-ref (@cells interp)
                         (@cellptr interp)))
      (interpreter
        (@tape interp) (match-token (@tape interp) (+ 1 (@tapeptr interp)) 0)
        (@cells interp) (@cellptr interp) (@ops interp))
      (interpreter (@tape interp) (+ 1 (@tapeptr interp))
        (@cells interp) (@cellptr interp) (@ops interp))))

  (define (endop interp)
    (define (match-token tape tapeptr depth)
      (unless (>= tapeptr 0) (error "Unmatched ']' token"))
      (cond ((eq? (list-ref tape tapeptr) endtoken)
             (match-token tape (- tapeptr 1) (+ depth 1)))
            ((eq? (list-ref tape tapeptr) starttoken)
             (if (= 0 depth) tapeptr
               (match-token tape (- tapeptr 1) (- depth 1))))
            (else (match-token tape (- tapeptr 1) depth))))

    (if (not (eq? 0 (list-ref (@cells interp)
                              (@cellptr interp))))
      (interpreter
        (@tape interp) (match-token (@tape interp) (- (@tapeptr interp) 1) 0)
        (@cells interp) (@cellptr interp) (@ops interp))
      (interpreter (@tape interp) (+ 1 (@tapeptr interp))
        (@cells interp) (@cellptr interp) (@ops interp))))

  (cond ((eq? starttoken token) (startop interp))
        ((eq? endtoken token) (endop interp))
        (else (error "How did I get here?"))))

(define (interpret interp)
  (define (match-ops ops index interp token)
    (if (>= index (length ops))
      (interpreter (@tape interp) (+ 1 (@tapeptr interp))
        (@cells interp) (@cellptr interp) (@ops interp))
      (letrec ((op (list-ref ops index))
               (literal (operation-literal op))
               (function (operation-function op)))
         (if (eq? literal token) (function interp token)
           (match-ops ops (+ 1 index) interp token)))))

  (let ((tape  (@tape interp))  (tapeptr (@tapeptr interp))
        (cells (@cells interp)) (cellptr (@cellptr interp))
        (ops   (@ops interp)))
    (if (>= tapeptr (length tape)) (void)
      (let ((token (list-ref tape tapeptr)))
        (interpret (match-ops ops 0 interp token))))))

(define (start-interpret tape ops)
  (interpret (interpreter (string->list tape) 0 (make-list NUMCELLS 0) 0 ops)))

(define (start-repl ops)
  (display "\n>>> ")
  (let ((input (read-line)))
    (if (equal? input "exit") (exit) (start-interpret input ops))))

(let ((argv (current-command-line-arguments)))
  (let ((ops (list (operation #\+ incop)   (operation #\- decop)
                   (operation #\< leftop)  (operation #\> rightop)
                   (operation #\. printop) (operation #\, readop)
                   (operation #\[ loop)    (operation #\] loop))))
    (cond ((> (vector-length argv) 1) (error "Please provide 0 or 1 arguments"))
          ((= (vector-length argv) 1) (start-interpret (vector-ref argv 0) ops))
          (else (start-repl ops)))))
