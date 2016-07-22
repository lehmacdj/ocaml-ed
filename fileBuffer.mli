(*
 * A text buffer being used by oed.
 *)

(*
 * The type of the text buffer.
 *)
type t

(*
 * Creates a buffer that represents the file with its name given by a string.
 * If no string is provided an unamed buffer is created instead.
 *)
val make: string option -> t

(*
 * Commands for manipulating text in the buffer, writing the buffer, etc.
 *)


(*
 * Returns the line of text given by the line number. Returns None if that line
 * is out of range for the buffer.
 *)
val get: int -> string option
