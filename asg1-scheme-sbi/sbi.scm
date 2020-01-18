#!/usr/bin/mzscheme -qr
;;#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzschee -qr
;; $Id: sbi.scm,v 1.12 2020-01-08 17:13:13-08 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;
;; I got this func_table starter code from Mackey's Example Directory
(define PRINT (lambda (x)
               (display x)
               (newline)
              )
)

(define func_table (make-hash))
(define label_hash (make-hash))
(define append_to_lh (lambda (line) (if (not (equal? ((cdr line) '())))
                                     (hash-set! label_hash (car line) (cadr line))
                                     ( '())
                                     )
                     )
)
(define append_to_lh (lambda (line) (if (not (equal? (cdr line) '()))
                                        (hash-set! label_hash (car line) (cadr line))
                                        (display "")
                                        )
                      )
)

(for-each
    (lambda (item) (hash-set! func_table (car item) (cadr item)))
    `(("print" ,(lambda (x) (display x) (newline))))
;;      ("INPUT" )
;;      ("DIM" )
;;      ("LET" )
;;      ("IF" )
;;      ("GOTO" ))
)
 
(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))


(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (for-each (lambda (line) (append_to_lh line) ) program)
    (printf ")~n"))

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program))))

(if (terminal-port? *stdin*)
    (main (vector->list (current-command-line-arguments)))
    (printf "sbi.scm: interactive mode~n"))

(display label_hash)
