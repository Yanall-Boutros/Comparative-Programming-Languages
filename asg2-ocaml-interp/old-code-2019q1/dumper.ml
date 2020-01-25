(* $Id: dumper.ml,v 1.15 2019-01-25 17:43:51-08 - - $ *)

let quote string =
    let regex = Str.regexp "\""
    and subst _ = "\\\""
    in  "\"" ^ Str.global_substitute regex subst string ^ "\""

let join start sep stop list =
    let rec join' list' = match list' with
        | [] -> stop
        | [unit] -> unit ^ stop
        | head::tail -> head ^ sep ^ " " ^ join' tail
    in match list with
        | [] -> start ^ stop
        | _::_ -> start ^ join' list

let string_of_option str_fn item = match item with
    | None -> "None"
    | Some thing -> "Some (" ^ str_fn thing ^ ")"

let string_of_ctor ctor args =
    join (ctor ^ " (") "," ")" args

let string_of_list str_fn list =
    join "[" ";" "]" (List.map str_fn list)

let rec string_of_printable printable = match printable with
    | Absyn.Printexpr expr ->
          string_of_ctor "Printexpr" [string_of_expr expr]
    | Absyn.String string ->
          string_of_ctor "String" [quote string]

and string_of_memref memref = match memref with
    | Absyn.Arrayref (ident, expr) ->
          string_of_ctor "Arrayref" [quote ident; string_of_expr expr]
    | Absyn.Variable ident -> string_of_ctor "Variable" [quote ident]

and string_of_expr expr = match expr with
    | Absyn.Number number ->
          string_of_ctor "Number" [string_of_float number]
    | Absyn.Memref memref ->
          string_of_ctor "Memref" [string_of_memref memref]
    | Absyn.Unary (oper, expr) ->
          string_of_ctor "Unary" [quote oper; string_of_expr expr]
    | Absyn.Binary (oper, expr1, expr2) ->
          string_of_ctor "Binary"
              [quote oper; string_of_expr expr1; string_of_expr expr2]


let string_of_stmt (stmt: Absyn.stmt) = match stmt with
    | Absyn.Dim (ident, expr) ->
          string_of_ctor "Dim"
              [quote ident ^ ", " ^ string_of_expr expr]
    | Absyn.Let (memref, expr) ->
          string_of_ctor "Let"
              [string_of_memref memref; string_of_expr expr]
    | Absyn.Goto label ->
          string_of_ctor "Goto" [quote label]
    | Absyn.If (expr, label) ->
          string_of_ctor "If" [string_of_expr expr; quote label]
    | Absyn.Print printable'list ->
          string_of_ctor "Print"
              [string_of_list string_of_printable printable'list]
    | Absyn.Input memref'list ->
          string_of_ctor "Input"
              [string_of_list string_of_memref memref'list]

let dump_progline (linenr, label'option, stmt'option) =
    Printf.fprintf stderr "%d %s: %s\n%!" linenr
        (string_of_option quote label'option)
        (string_of_option string_of_stmt stmt'option)

let dump_program (program : Absyn.program) =
    List.iter dump_progline program

