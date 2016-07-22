(*
 * A text buffer being used by oed.
 *)

(*
 * The type of the text buffer.
 *)
type t

(*
 * Creates a buffer that represents the file with its name given by a string.
 *)
val make: string -> t

(*
 * Commands for manipulating text in the buffer, writing the buffer, etc.
 *)
