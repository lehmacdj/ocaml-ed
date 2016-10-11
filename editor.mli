(*
 * Handles the processing of strings and the output of result strings.
 *)

(**
 * import the command type so it may be used without prefix
 *)
type command = EdCommand.t

(**
 * The type that represents a text editor.
 *)
type t

(**
 * Create a new text editor in the input state.
 *)
val make: string -> t

(**
 * Execute a command on this editor and return the new state of the editor.
 *)
val execute: t -> command -> t

(**
 * The error string that should be printed to std::out if in verbose mode
 *)
val out_string: t -> string

(**
 * Whether or not to verbosely output the out_string
 *)
val verbose: t -> bool
