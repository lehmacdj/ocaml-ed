(*
 * A text buffer being used by oed.
 *)
open Core.Std
open Types

(**
 * The type of the text buffer. Represents the filename and
 *)
type t

(** an error the file buffer can return *)
type buffer_error

(** a string representation of the error *)
val string_of_buffer_error: buffer_error -> string

(**
 * Creates a buffer that represents the file with its name given by a string.
 * If no string is provided an unamed buffer is created instead.
 *)
val make: string option -> t

(**
 * Return a buffer with name changed
 *)
val set_name: t -> string -> (t, buffer_error) Result.t

(**
 * Return the name of the buffer, is None if the buffer doesn't have a name
 *)
val name: t -> string option

(**
 * Returns the line of text given by the line number. Returns None if that line
 * is out of range for the buffer.
 *)
val get: t -> int -> (string, buffer_error) Result.t

(**
 * Writes the file to the filesytem. Does not affect the buffer in any other
 * way.
 *)
val write: t -> range:(int * int) -> (unit, buffer_error) Result.t

(** Return the number of lines in the FileBuffer *)
val line_count: t -> int

(** Return the text from lines start to end *)
val lines: t -> range:(int * int) -> (string list, buffer_error) Result.t

(**
 * Delete the lines specified by range (inclusive) from the file.
 * Precondition: range must be a pair of integers with the first smaller than
 * the second, otherwise behavior is undefined
 *)
val delete: t -> range:(int * int) -> (t, buffer_error) Result.t

(**
 * insert a line to the buffer at the specified address
 *)
val insert: t -> at:int -> lines:text -> (t, buffer_error) Result.t

(**
 * The direction to search through the buffer.
 *)
type search_direction =
  | Forward (** Search forward through the buffer to lines past the current *)
  | Backward (** Search backward for previous lines *)

(**
 * Returns the next line of text that after or before the current line marker.
 *)
val find: t -> int -> string -> direction:search_direction -> (int, buffer_error) Result.t
