(* $Id: main.ml,v 1.3 2020-01-24 12:57:06-08 - - $ *)

(*
* Main program reads a file and prints to stdout.
*)

let interpret_source filename =
    try (let sourcefile =
             if filename = "-"
             then stdin
             else open_in filename in
         let lexbuf = Lexing.from_channel sourcefile in
         let abstract_syntax = Parser.program Scanner.token lexbuf in
         Interp.interpret_program abstract_syntax)
    with Sys_error (string) -> Etc.die [string]

let _ = if !Sys.interactive
        then ()
        else match Array.length Sys.argv with
             | 1 -> interpret_source "-"
             | 2 -> interpret_source Sys.argv.(1)
             | _ -> Etc.usage_exit ["[filename.sb]"]

