#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/ocaml/bin/ocaml
(* $Id: printfloat.ml,v 1.5 2020-01-17 14:30:03-08 - - $ *)

let prt n = (print_float n; Printf.printf "  %.15g\n%!" n);;

let pi = acos ~-.1.0;;
let e = exp 1.0;;

let numbers = [
   1.0;
   1.0 /. 3.0;
   12345678901234567890.0;
   0.0 /. 0.0;
   3.0 /. 0.0;
   pi;
   e;
];;

List.map prt numbers;;
