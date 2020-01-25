(* $Id: absyn.mli,v 1.3 2020-01-22 16:07:24-08 - - $ *)

(*
* Abstract syntax definitions for SB.
*)

type linenr    = int
type ident     = string
type label     = string
type number    = float
type oper      = string

and  memref    = Arrayref of ident * expr
               | Variable of ident

and  expr      = Number of number
               | Memref of memref
               | Unary of oper * expr
               | Binary of oper * expr * expr

type printable = Printexpr of expr
               | String of string

type stmt      = Dim of ident * expr
               | Let of memref * expr
               | Goto of label
               | If of expr * label
               | Print of printable list
               | Input of memref list

type progline  = linenr * label option * stmt option

type program   = progline list

