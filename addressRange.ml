(*
 * Implementation of address.mli
 *)
open Core.Std

(**
 * An address is a pair of lines that represents the first and last line of the
 * program inclusively.
 *)
type t = int * int

let make str cur =
  (* Parse the string return the range; encapsulated *)
  failwith "unimplemented"

let first (f, _) = f

let last (_, l) = l

let range addr = addr

let length (f, l) =
  l - f + 1
