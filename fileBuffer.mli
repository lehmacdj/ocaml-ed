(*
 * A text buffer being used by oed.
 *)
open Types

(**
 * The type of the text buffer. Represents the filename and
 *)
type t

(**
 * Creates a buffer that represents the file with its name given by a string.
 * If no string is provided an unamed buffer is created instead.
 *)
val make: string option -> t

(**
 * Return a buffer with name changed
 *)
val set_name: t -> string -> t

(**
 * Returns the line of text given by the line number. Returns None if that line
 * is out of range for the buffer.
 *)
val get: t -> int -> string option

(**
 * Writes the file to the filesytem.
 *)
val write: t -> ?range:address_range -> unit

(**
 * The direction to search through the buffer
 *)
type search_direction =
  | Forward (** Search forward through the buffer to lines past the current *)
  | Backward (** Search backward for previous lines *)

(**
 * Returns the next line of text that after or before the current line marker.
 *)
val find: t -> int -> string -> search_direction -> int
