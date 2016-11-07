(*
 * Handles the processing of strings and the output of result strings.
 *)

(**
 * The type that the editor returns
 *)
type editor_response =
  | Nothing (** the editor should return no response *)
  | UnspecifiedError (** the editor has an error but there is no message *)
  | EdError of string (** the editor has an error with a message *)
  | ByteCount of int (** the byte count of a file loaded or written *)
  | Text of string

(**
 * The type that represents a text editor.
 *)
type t

(**
 * Create a new text editor in the input state.
 *)
val make: string -> t

(**
 * Returns true if the editor is running.
 *)
val running: t -> bool

(**
 * Execute a command on this editor and return the new state of the editor.
 *)
val execute: t -> EdCommand.t -> t * editor_response

(**
 * Return the editor response for a parse error current state of the editor.
 *)
val response: t -> parse_error:EdParser.parse_error -> editor_response
