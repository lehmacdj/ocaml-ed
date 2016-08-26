(*
 * Handles the processing of strings and the output of result strings.
 *)

(**
 * The type that represents a text editor.
 *)
type t

(**
 * Create a new text editor in the input state.
 *)
val make: string -> t

(**
 * Returns a editor that has been transformed after applying the input string.
 *)
val process_string: t -> string -> t

val out_string: t -> string

val verbose: t -> bool
