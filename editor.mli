(*
 * Handles the processing of strings and the output of result strings.
 *)

(**
 * The type that represents a text editor.
 *)
type t

(**
 * The type that the editor returns
 *)
type editor_response =
  | Nothing (** the editor should return no response *)
  | UnknownError (** the editor has an error but there is no message *)
  | EdError of string (** the editor has an error with a message *)
  | ByteCount of int (** the byte count of a file loaded or written *)

(**
 * Create a new text editor in the input state.
 *)
val make: string -> t

(**
 * Execute a command on this editor and return the new state of the editor.
 *)
val execute: t -> EdCommand.t -> t * editor_response
