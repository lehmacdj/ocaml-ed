(*
 * The interface for an address
 *)
open Core.Std

(**
 * The type of the address. This represents a range of lines that a given
 * command operates on.
 *)
type t

(**
 * Creates an address based on the string representing an address and the
 * current line number.
 *)
val make: string -> int -> t

(**
 * Returns the first line number of the address.
 *)
val first: t -> int

(**
 * Returns the last line number of the address.
 *)
val last: t -> int

(**
  * Returns the start end end of the address.
  *)
val range: t -> int * int

(**
 * The amount of lines in the addressed region.
 *)
val length: t -> int
