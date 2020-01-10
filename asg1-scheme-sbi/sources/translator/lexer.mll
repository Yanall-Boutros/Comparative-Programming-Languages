(* $Id: lexer.mll,v 1.4 2019-11-26 14:07:46-08 - - $ *)

{
(******** BEGIN LEXER SEMANTICS ********)

open Absyn
open Etc
open Lexing
open Parser
open Printf

let lexerror lexbuf =
    lexeprint (lexeme_start_p lexbuf)
              ["invalid character `" ^ (lexeme lexbuf) ^ "'"]

let newline lexbuf =
    let incrline pos =
        {pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum}
    in  (lexbuf.lex_start_p <- incrline lexbuf.lex_start_p;
         lexbuf.lex_curr_p <- incrline lexbuf.lex_curr_p)

let list lexbuf =
    let pos = lexeme_start_p lexbuf
    in  (if pos.pos_bol = pos.pos_cnum
         then printf ";;%4d: " pos.pos_lnum;
         printf "%s" (lexeme lexbuf))

(******** END LEXER SEMANTICS ********)
}

let letter          = ['a'-'z' 'A'-'Z' '_']
let digit           = ['0'-'9']
let fraction        = (digit+ '.'? digit* | '.' digit+)
let exponent        = (['E' 'e'] ['+' '-']? digit+)

let comment         = ('#' [^'\n']*)
let ident           = (letter (letter | digit)*)
let number          = (fraction exponent?)
let string          = '"' [^'\n' '"']* '"'

rule token          = parse
    | eof           { EOF }
    | [' ' '\t']    { list lexbuf; token lexbuf }
    | comment       { list lexbuf; token lexbuf }
    | "\n"          { list lexbuf; newline lexbuf; EOL }
    | ":"           { list lexbuf; COLON }
    | ","           { list lexbuf; COMMA }
    | "("           { list lexbuf; LPAR }
    | ")"           { list lexbuf; RPAR }
    | "["           { list lexbuf; LSUB }
    | "]"           { list lexbuf; RSUB }
    | "="           { list lexbuf; EQUAL (lexeme lexbuf) }
    | "!="          { list lexbuf; RELOP (lexeme lexbuf) }
    | "<"           { list lexbuf; RELOP (lexeme lexbuf) }
    | "<="          { list lexbuf; RELOP (lexeme lexbuf) }
    | ">"           { list lexbuf; RELOP (lexeme lexbuf) }
    | ">="          { list lexbuf; RELOP (lexeme lexbuf) }
    | "+"           { list lexbuf; ADDOP (lexeme lexbuf) }
    | "-"           { list lexbuf; ADDOP (lexeme lexbuf) }
    | "*"           { list lexbuf; MULOP (lexeme lexbuf) }
    | "/"           { list lexbuf; MULOP (lexeme lexbuf) }
    | "%"           { list lexbuf; MULOP (lexeme lexbuf) }
    | "^"           { list lexbuf; POWOP (lexeme lexbuf) }
    | "dim"         { list lexbuf; DIM }
    | "goto"        { list lexbuf; GOTO }
    | "if"          { list lexbuf; IF }
    | "input"       { list lexbuf; INPUT }
    | "let"         { list lexbuf; LET }
    | "print"       { list lexbuf; PRINT }
    | number        { list lexbuf; NUMBER (lexeme lexbuf) }
    | string        { list lexbuf; STRING (lexeme lexbuf) }
    | ident         { list lexbuf; IDENT (lexeme lexbuf) }
    | _             { list lexbuf; lexerror lexbuf; token lexbuf }

