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
(define array_table (make-hash))
;; ====================================================================
;; Function Definitions
;; ====================================================================
;; Props to mackey for the code
(define *symbol-table* (make-hash))
(define (symbol-get key)
        (hash-ref *symbol-table* key))
(define (symbol-put! key value)
        (hash-set! *symbol-table* key value))

(for-each
    (lambda (pair)
            (symbol-put! (car pair) (cadr pair)))
    `(
        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+       ,+)
        (^       ,expt)
        (ceil    ,ceiling)
        (exp     ,exp)
        (floor   ,floor)
        (log     ,log)
        (sqrt    ,sqrt)
        (let    ,"let")
        (input  ,"input")
        (print  ,"print")
        (dim    ,"dim")
        (if     ,"if")
        (goto   ,"goto")
        (eof    , 0)
     ))

(define asub (lambda (a . i)
  (vector-ref (hash-ref var_table a) i)
)
)

(define PRINT (lambda (l)
               ;; if the value is null, print new line
               (if (not (eq? l '()))(cond
                                          [(string? (car l))(display (car l))(PRINT (cdr l))]
                                          [(number? (car l))(display (car l))(PRINT (cdr l))]
               ;; If the element is not a string, evaluate the expression and print
                                          [(list? (car l))(display (EVAL_EXPR (car l) ))(PRINT (cdr l))]
                                          ) 
               ;; otherwise, -- if the first element is string print string.
               (newline)
              )
              '()
              )
)
(define (EVAL_EXPR expr)
;; Starter Code taking from mackeys evalexpr.scm
    (cond ((number? expr) (+ 0.0 expr))
          ((symbol? expr) (hash-ref var_table expr))
          (else (let ((fn (hash-ref func_table (car expr)))
                      (args (map EVAL_EXPR (cdr expr))))
                     (apply fn args))))
)
;; The dim statement creates an array given by the variable name and
;; inserts it into the array table, replacing any previous array already in
;; the array table. The dimension of the array is given by the expression.
;; All values in the array are initialized to 0. The expression is rounded to
;; the nearest integer before being used as the bound, which must be positive
(define DIM (lambda (l)
             (define var_name (cadar l))
             (define var_size (caddar l))
             (define vec (make-vector (exact-round (EVAL_EXPR var_size))))
             (hash-set! array_table var_name vec)
             '()
             )
)

;; A let statement makes an assignment to a variable. The expression is
;; first evaluated. For a Variable, its value is stored into the Symbol table,
;; replacing whatever was there previously. For an Arrayref , the store mes-
;; sage is sent to the vector representing the array. If the Symbol table
;; entry is not an array, an error occurs.
;; got some help for this part from 
;; https://github.com/bosdhill/CS112/blob/master/asg1/sbi.scm
;; I got lost with how to handle the array part of the function. Additionally
;; since I implemented this program in a different order I had to change 
;; some of the logic surrouding evaluating expressions
(define LET (lambda (l)
             (cond
             ;; Determine if array or variable
             [(symbol? (car l))(hash-set! var_table (car l) (EVAL_EXPR (cadr l)))]
             ;; If not variable, figure out what to do for array
             [(pair? (car l))
             ;; If it is a pair, then check to see if the array is in the table,
             ;; and check to see if the array is in bounds
             ;; check to see if it's in table
               (if (and (hash-has-key? array_table (car l))
                         (<= (- (EVAL_EXPR (cadr l)) 1)
                             (vector-length (car l))
                         )
                    )
                    (vector-set! (hash-ref array-table (car l))
                                 (exact-round (- (EVAL_EXPR (cadr l)) 1))
                                 (EVAL_EXPR (car (cddr l)))
                    )
                    (printf "Vector not found or array out of bounds")
               )
             ]
             [else (printf "Error, improper LET usage")]
             )
             '()
             )
)

;; Numeric values are read in and assigned to the input variables in
;; sequence. Arguments might be elements of an array. For each value
;; read into a Memory, the value is inserted into the Symbol table under
;; that variable’s key. For arrays, the array must already exist and the sub-
;; script not be out of bounds.

;; If an invalid value (anything that is not a number?) is read, the value
;; returned is nan. If end of file is encountered, the value returned is nan
;; and the variable eof is entered into the symbol table with the value 1.
;; The value of nan can be computed using the expression (/ 0.0 0.0).
;; Counterintuitively, the expression (= nan nan) is false.

;; Example code taken from mackeys example folder
(define INPUT(lambda (l)
             (define cur_symbol (car l))
             (let ((object (read)))
             (cond [(eof-object? object) object]
                   [(number? object)
                   ;; If the argument is an element of array, then
                   ;; check if it exists and is in correct bounds, 
                   ;; then update set array sub value to object.
                   ;; Take the cdr of input args and repeat
                       (cond 
                          [(symbol? cur_symbol)(hash-set! var_table cur_symbol object)]
                          [else (begin (if
                                         (and (hash-has-key? array_table (car l))
                                              (<= (- (EVAL_EXPR (cadr l)) 1)
                                              (vector-length (car l)))
                                         )
                                         (vector-set! (hash-ref array_table (car l))
                                                      object
                                         )
                                         (die "Vector doesn't exist")
                                )
                            )
                          ]
                        )
                   ;; otherwise, set varialbe to value
                   ]
                   [else (begin (die "invalid number: ~a~n")
                                (hash-set! var_table eof 1)
                                (/ 0.0 0.0)
                          )
                   ]
              )
             ) 
             '()
             )
)
(define IF (lambda (l)
             (define relexpr (car l))
             (define target (cadr l))
             (newline)
             (newline)
             (display relexpr)
             (newline)
             (define e (EVAL_EXPR relexpr))
             '()
          )
 )
(define GOTO (lambda (l)
             (if (hash-has-key? label_hash (car l))
                 (interpret-program (hash-ref label_hash (car l)))
                 (die "Bad Label")
             )
             '()
          )
 )

;; Again, thanks to Mackey for some example code
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
   [(null? (cdr l) ) (hash-set! label_hash (car l) '())]
   ;; if the next element is null, append and break
   [(eq? (what-kind (car l)) 'other) (hash-set! label_hash (car l) (cadr l)) (insert_labels (cdr l))]
   [else (insert_labels (cdr l))]
   )
  )
)

(define interpret-statement (lambda (l)
    ;;If there is a statement, lookup the keyword in the statement hash
    ;; lookup the car of l  to get the keyword
    (define cmd (hash-ref *symbol-table* (caar l)))
    (if (hash-has-key? func_table cmd)
        ((hash-ref func_table cmd) (cdar l))
        (die "Error: no key")
    )
  )
)

(define interpret-program (lambda (l)
   ;; if there is no statement, call the function recursively on cdr
   (if (empty? l) (exit) '())
   (if (eq? (what-kind (car l)) 'list)(hash-set! var_table "ret-val" (interpret-statement l ))(interpret-program (cdr l)))
   (if (null? (hash-ref var_table "ret-val")) (interpret-program (cdr l)) "")
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
    `(("print" , PRINT)
      ("input" , INPUT)
      ("dim" , DIM)
      ("let" , LET)
      ("if" , IF)
      ("goto" ,GOTO)
      ;; operator section
      (+  ,+) 
      (-  ,-) 
      (*  ,*)
      (/ , (lambda (x y) (/ x  y )))
      (% , (lambda (x y) (- (+ x 0.0) 
      (* (truncate (/ (+ x 0.0) (+ y 0.0))) (+ y 0.0)))))
      (^ , expt)
      ;; relop
      (= , equal?)
      (< , <)
      (> , >)
      (!= , (lambda (x y) (not (equal? x y))))
      (>= , >=)
      (<= , <=)
      ;; math functions 
      (exp , exp)
      (ceil , ceiling)
      (floor , floor)
      (sqrt , sqrt)
      (abs , abs)
      (acos , acos)
      (asin , asin)
      (atan , atan)
      (cos , cos)
      (round , round)
      (sin , sin)
      (tan , tan)
      (trunc , truncate)
      (log , (lambda (x) (log (+ x 0.0))))
      (log10 , (lambda (x) (/ (log (+ x 0.0)) (log 10.0))))
      (log2 , (lambda (x) (/ (log (+ x 0.0)) (log 2.0))))
      (asub, asub)
      (eof,  eof)
  )
)
 
(for-each
    (lambda (item) (hash-set! var_table (car item) (cadr item)))
    `(
      ("lines" , '())
      ("ret-val" 0)
      (eof,      0)
      )
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
    (for-each (lambda (line) (hash-set! var_table "lines" (list_append (hash-ref var_table "lines") line)) )program)
)

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
(interpret-program (hash-ref var_table "lines"))
