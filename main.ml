(*
 * Implemets main.mli.
 *)
open Core.Std

(*
 * Imperative commands allowed here (but nowhere else).
 *)
let rec run buffer =
  let input = read_line () in
  print_endline input;
  run buffer
