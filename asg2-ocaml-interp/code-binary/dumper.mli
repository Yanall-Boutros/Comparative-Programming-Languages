(* Generated: Fri Jan 24 14:55:58 PST 2020 *)
val quote : string -> string
val join : string -> string -> string -> string list -> string
val string_of_option : ('a -> string) -> 'a option -> string
val string_of_ctor : string -> string list -> string
val string_of_list : ('a -> string) -> 'a list -> string
val string_of_printable : Absyn.printable -> string
val string_of_memref : Absyn.memref -> string
val string_of_expr : Absyn.expr -> string
val string_of_stmt : Absyn.stmt -> string
val dump_progline : int * string option * Absyn.stmt option -> unit
val dump_program : Absyn.program -> unit
