#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/ocaml/bin/ocaml
(* $Id$ *)

let prt n = (print_float n; Printf.printf "%.15g\n%!" n)

map prt [1.; 1./3.; 12345678901234567890.; 0.0/.0.0; 3.0/.0.0]

