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

;; ====================================================================
;; Variable Declarations
;; ====================================================================
(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))
;; I got this func_table starter code from Mackey's Example Directory
(define func_table (make-hash))
(define var_table (make-hash))
(define line_hash (make-hash))
(define label_hash (make-hash))
;; ====================================================================
;; Function Definitions
;; ====================================================================
(define append_to_lh (lambda (line) (if (not (null? (cdr line)))
                                        (hash-set! line_hash (car line) (cadr line))
                                        (display "")
                                        )
                      )
)
(define PRINT (lambda (x)
               (display x)
               (newline)
              )
)

(define ret-val-index (lambda (h i)
                 (if (hash-has-key? h i)
                  i
                  (ret-val-index h (+ i 1))
                  )
                 )
)
;; Again, thanks to Macket for some example code
(define (what-kind value)
    (cond ((real? value) 'real)
          ((vector? value) 'vector)
          ((procedure? value) 'procedure)
          ((list? value) 'list)
          (else 'other)))


(define insert_labels (lambda (l)
  ;; if the car is null, return empty list
  ;; if the car of the list is of type other, append the cadr
  ;; else, recur
  (cond 
   [(null? (cdr l) ) (hash-set! label_hash (car l) (car l))]
   ;; if the next element is null, append and break
   [(eq? (what-kind (car l)) 'other) (hash-set! label_hash (car l) (cadr l)) (insert_labels (cdr l))]
   [else (insert_labels (cdr l))]
   )
  )
)



;; Thanks to
;; https://stackoverflow.com/questions/3172768/adding-an-element-to-list-in-scheme
;; for the help
(define (list_append . lsts)
  (cond
    ((null? lsts) '())
    ((null? (car lsts)) (apply append (cdr lsts)))
    (else (cons (caar lsts) (apply append (cdar lsts) (cdr lsts))))))

;;(define interpret-program (lambda tllist)
;;;; If there is no statement, call interpret statement with the cdr
;;;; of the top level node
;;                           ()
;;
;;)

;; ====================================================================
;; Build the tables
;; ====================================================================
(for-each
    (lambda (item) (hash-set! func_table (car item) (cadr item)))
    `(("print" ,(lambda (x) (display x) (newline))))
;;      ("INPUT" )
;;      ("DIM" )
;;      ("LET" )
;;      ("IF" )
;;      ("GOTO" ))
)
 
(for-each
    (lambda (item) (hash-set! var_table (car item) (cadr item)))
    `(("lines" , '()))
)

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
    (for-each (lambda (line) (hash-set! var_table "lines" (list_append (hash-ref var_table "lines") line)) )program)
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

;; (display (hash-ref var_table "lines"))
;; Build the table which contains all key values such that
;; key = label, value = top-level node pointed at
(insert_labels (hash-ref var_table "lines"))
(display label_hash)
