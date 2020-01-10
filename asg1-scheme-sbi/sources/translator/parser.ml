type token =
  | RELOP of (string)
  | EQUAL of (string)
  | ADDOP of (string)
  | MULOP of (string)
  | POWOP of (string)
  | IDENT of (string)
  | NUMBER of (string)
  | STRING of (string)
  | COLON
  | COMMA
  | LPAR
  | RPAR
  | LSUB
  | RSUB
  | EOL
  | EOF
  | DIM
  | LET
  | GOTO
  | IF
  | PRINT
  | INPUT

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
(******** BEGIN PARSER SEMANTICS ********)

open Absyn
open Etc
open Lexing

let syntax () = lexeprint (symbol_start_pos ()) ["syntax error"]

let linenr () = (symbol_start_pos ()).pos_lnum

(******** END PARSER SEMANTICS ********)
# 40 "parser.ml"
let yytransl_const = [|
  265 (* COLON *);
  266 (* COMMA *);
  267 (* LPAR *);
  268 (* RPAR *);
  269 (* LSUB *);
  270 (* RSUB *);
  271 (* EOL *);
    0 (* EOF *);
  272 (* DIM *);
  273 (* LET *);
  274 (* GOTO *);
  275 (* IF *);
  276 (* PRINT *);
  277 (* INPUT *);
    0|]

let yytransl_block = [|
  257 (* RELOP *);
  258 (* EQUAL *);
  259 (* ADDOP *);
  260 (* MULOP *);
  261 (* POWOP *);
  262 (* IDENT *);
  263 (* NUMBER *);
  264 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\004\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\011\000\011\000\013\000\013\000\012\000\012\000\008\000\008\000\
\007\000\010\000\010\000\009\000\009\000\014\000\014\000\015\000\
\015\000\016\000\016\000\016\000\016\000\016\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\002\000\003\000\003\000\000\000\002\000\001\000\001\000\000\000\
\002\000\002\000\004\000\002\000\004\000\002\000\001\000\002\000\
\003\000\001\000\001\000\001\000\003\000\001\000\001\000\001\000\
\004\000\003\000\003\000\003\000\001\000\003\000\001\000\003\000\
\001\000\003\000\002\000\001\000\001\000\004\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\004\000\000\000\046\000\000\000\000\000\039\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\000\
\000\000\003\000\040\000\044\000\041\000\042\000\045\000\043\000\
\000\000\010\000\000\000\024\000\000\000\012\000\000\000\036\000\
\000\000\000\000\037\000\000\000\000\000\000\000\031\000\000\000\
\020\000\000\000\014\000\000\000\000\000\016\000\002\000\000\000\
\000\000\000\000\000\000\000\000\000\000\005\000\009\000\000\000\
\000\000\035\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\034\000\000\000\
\000\000\000\000\000\000\013\000\030\000\032\000\017\000\021\000\
\025\000\038\000"

let yydgoto = "\002\000\
\003\000\004\000\014\000\015\000\016\000\034\000\028\000\035\000\
\042\000\037\000\043\000\046\000\044\000\038\000\039\000\040\000"

let yysindex = "\019\000\
\000\000\000\000\000\000\001\000\037\255\000\000\000\000\015\255\
\015\255\015\255\060\255\008\255\015\255\040\255\105\255\000\000\
\014\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\062\255\000\000\062\255\000\000\070\255\000\000\060\255\000\000\
\060\255\057\255\000\000\036\255\055\255\083\255\000\000\084\255\
\000\000\085\255\000\000\080\255\082\255\000\000\000\000\015\255\
\015\255\015\255\060\255\008\255\015\255\000\000\000\000\060\255\
\060\255\000\000\039\255\060\255\060\255\060\255\060\255\015\255\
\060\255\060\255\008\255\015\255\255\254\085\255\000\000\053\255\
\085\255\085\255\083\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\079\255\000\000\000\000\000\000\088\255\
\089\255\095\255\096\255\003\255\097\255\000\000\092\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\059\255\000\000\000\000\000\000\000\000\000\000\
\000\000\081\255\000\000\000\000\000\000\117\255\000\000\099\255\
\000\000\007\255\000\000\037\255\093\255\000\000\000\000\000\000\
\000\000\000\000\000\000\100\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\101\255\000\000\000\000\
\094\255\110\255\135\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\095\000\252\255\002\000\250\255\
\253\255\000\000\063\000\065\000\000\000\071\000\231\255\108\000"

let yytablesize = 278
let yytable = "\017\000\
\007\000\063\000\029\000\025\000\027\000\030\000\045\000\036\000\
\027\000\026\000\031\000\045\000\081\000\006\000\032\000\041\000\
\019\000\015\000\033\000\001\000\006\000\019\000\055\000\019\000\
\020\000\021\000\022\000\023\000\024\000\059\000\019\000\020\000\
\021\000\022\000\023\000\024\000\061\000\062\000\063\000\077\000\
\078\000\063\000\029\000\025\000\027\000\030\000\045\000\036\000\
\027\000\026\000\071\000\018\000\069\000\070\000\047\000\063\000\
\072\000\073\000\074\000\076\000\023\000\045\000\031\000\027\000\
\082\000\006\000\032\000\060\000\023\000\056\000\033\000\057\000\
\064\000\023\000\056\000\019\000\020\000\021\000\022\000\023\000\
\024\000\023\000\023\000\023\000\023\000\023\000\065\000\063\000\
\066\000\067\000\023\000\068\000\023\000\008\000\023\000\023\000\
\040\000\044\000\023\000\033\000\033\000\033\000\033\000\041\000\
\042\000\043\000\007\000\022\000\033\000\054\000\033\000\026\000\
\033\000\033\000\015\000\011\000\033\000\029\000\029\000\029\000\
\048\000\049\000\050\000\051\000\052\000\053\000\029\000\027\000\
\029\000\079\000\029\000\029\000\080\000\075\000\029\000\028\000\
\028\000\028\000\058\000\000\000\000\000\000\000\000\000\000\000\
\028\000\000\000\028\000\000\000\028\000\028\000\000\000\000\000\
\028\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\005\000\000\000\000\000\000\000\000\000\000\000\006\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\008\000\009\000\010\000\011\000\012\000\013\000"

let yycheck = "\004\000\
\000\000\003\001\009\000\008\000\009\000\010\000\013\000\011\000\
\013\000\008\000\003\001\009\001\014\001\006\001\007\001\008\001\
\010\001\015\001\011\001\001\000\006\001\015\001\009\001\016\001\
\017\001\018\001\019\001\020\001\021\001\033\000\016\001\017\001\
\018\001\019\001\020\001\021\001\001\001\002\001\003\001\065\000\
\066\000\003\001\049\000\048\000\049\000\050\000\053\000\051\000\
\053\000\048\000\012\001\015\001\056\000\057\000\015\001\003\001\
\060\000\061\000\062\000\064\000\002\001\068\000\003\001\068\000\
\012\001\006\001\007\001\011\001\010\001\013\001\011\001\002\001\
\018\001\015\001\013\001\016\001\017\001\018\001\019\001\020\001\
\021\001\001\001\002\001\003\001\004\001\005\001\004\001\003\001\
\005\001\010\001\010\001\010\001\012\001\015\001\014\001\015\001\
\009\001\009\001\018\001\001\001\002\001\003\001\004\001\009\001\
\009\001\009\001\015\001\015\001\010\001\015\000\012\001\018\001\
\014\001\015\001\015\001\015\001\018\001\001\001\002\001\003\001\
\016\001\017\001\018\001\019\001\020\001\021\001\010\001\018\001\
\012\001\067\000\014\001\015\001\068\000\063\000\018\001\001\001\
\002\001\003\001\031\000\255\255\255\255\255\255\255\255\255\255\
\010\001\255\255\012\001\255\255\014\001\015\001\255\255\255\255\
\018\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\001\255\255\255\255\255\255\255\255\255\255\006\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001"

let yynames_const = "\
  COLON\000\
  COMMA\000\
  LPAR\000\
  RPAR\000\
  LSUB\000\
  RSUB\000\
  EOL\000\
  EOF\000\
  DIM\000\
  LET\000\
  GOTO\000\
  IF\000\
  PRINT\000\
  INPUT\000\
  "

let yynames_block = "\
  RELOP\000\
  EQUAL\000\
  ADDOP\000\
  MULOP\000\
  POWOP\000\
  IDENT\000\
  NUMBER\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 28 "parser.mly"
                                ( List.rev _1 )
# 242 "parser.ml"
               : Absyn.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    Obj.repr(
# 30 "parser.mly"
                                ( _2::_1 )
# 250 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmts) in
    Obj.repr(
# 31 "parser.mly"
                                ( syntax (); _1 )
# 257 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    Obj.repr(
# 32 "parser.mly"
                                ( [] )
# 263 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'label) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'action) in
    Obj.repr(
# 34 "parser.mly"
                                ( (linenr (), Some _1, Some _2) )
# 271 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'action) in
    Obj.repr(
# 35 "parser.mly"
                                ( (linenr (), None, Some _1) )
# 278 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'label) in
    Obj.repr(
# 36 "parser.mly"
                                ( (linenr (), Some _1, None) )
# 285 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "parser.mly"
                                ( (linenr (), None, None) )
# 291 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ident) in
    Obj.repr(
# 39 "parser.mly"
                                ( _1 )
# 298 "parser.ml"
               : 'label))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'array) in
    Obj.repr(
# 41 "parser.mly"
                                ( Dim (_2) )
# 305 "parser.ml"
               : 'action))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'memref) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 42 "parser.mly"
                                ( Let (_2, _4) )
# 314 "parser.ml"
               : 'action))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 43 "parser.mly"
                                ( Goto (_2) )
# 321 "parser.ml"
               : 'action))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'relexpr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 44 "parser.mly"
                                ( If (_2, _4) )
# 329 "parser.ml"
               : 'action))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'prints) in
    Obj.repr(
# 45 "parser.mly"
                                ( Print (_2) )
# 336 "parser.ml"
               : 'action))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
                                ( Print ([]) )
# 342 "parser.ml"
               : 'action))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'inputs) in
    Obj.repr(
# 47 "parser.mly"
                                ( Input (_2) )
# 349 "parser.ml"
               : 'action))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'print) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'prints) in
    Obj.repr(
# 49 "parser.mly"
                                ( _1::_3 )
# 357 "parser.ml"
               : 'prints))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'print) in
    Obj.repr(
# 50 "parser.mly"
                                ( [_1] )
# 364 "parser.ml"
               : 'prints))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 52 "parser.mly"
                                ( Printexpr (_1) )
# 371 "parser.ml"
               : 'print))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "parser.mly"
                                ( String (_1) )
# 378 "parser.ml"
               : 'print))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'memref) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'inputs) in
    Obj.repr(
# 55 "parser.mly"
                                ( _1::_3 )
# 386 "parser.ml"
               : 'inputs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'memref) in
    Obj.repr(
# 56 "parser.mly"
                                ( [_1] )
# 393 "parser.ml"
               : 'inputs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 58 "parser.mly"
                                ( Variable (_1) )
# 400 "parser.ml"
               : 'memref))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'array) in
    Obj.repr(
# 59 "parser.mly"
                                ( Array (_1) )
# 407 "parser.ml"
               : 'memref))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 61 "parser.mly"
                                ( (_1, _3) )
# 415 "parser.ml"
               : 'array))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 63 "parser.mly"
                                ( Binop (_2, _1, _3) )
# 424 "parser.ml"
               : 'relexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 64 "parser.mly"
                                ( Binop (_2, _1, _3) )
# 433 "parser.ml"
               : 'relexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 66 "parser.mly"
                                ( Binop (_2, _1, _3) )
# 442 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 67 "parser.mly"
                                ( _1 )
# 449 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 69 "parser.mly"
                                ( Binop (_2, _1, _3) )
# 458 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 70 "parser.mly"
                                ( _1 )
# 465 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'primary) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 72 "parser.mly"
                                ( Binop (_2, _1, _3) )
# 474 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary) in
    Obj.repr(
# 73 "parser.mly"
                                ( _1 )
# 481 "parser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                                ( _2 )
# 488 "parser.ml"
               : 'primary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'primary) in
    Obj.repr(
# 76 "parser.mly"
                                ( Unop (_1, _2) )
# 496 "parser.ml"
               : 'primary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 77 "parser.mly"
                                ( Constant (float_of_string (_1)) )
# 503 "parser.ml"
               : 'primary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'memref) in
    Obj.repr(
# 78 "parser.mly"
                                ( Memref (_1) )
# 510 "parser.ml"
               : 'primary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                                ( Fncall (_1, _3) )
# 518 "parser.ml"
               : 'primary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "parser.mly"
                                ( _1 )
# 525 "parser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
                                ( "dim" )
# 531 "parser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
                                ( "goto" )
# 537 "parser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
                                ( "if" )
# 543 "parser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
                                ( "input" )
# 549 "parser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                                ( "let" )
# 555 "parser.ml"
               : 'ident))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
                                ( "print" )
# 561 "parser.ml"
               : 'ident))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Absyn.program)
