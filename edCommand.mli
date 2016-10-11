(** Handles the parsing of all of the text passed into the text editor. *)

(** The type of a command which is passed around between various modules *)
type t =
  | Append of string list
  | Change of string list
  | Delete
  | Edit
  | File
  | Global
  | GlobalInteractive
  | HelpToggle
  | Help
  | Insert
  | Join
  | List
  | Move
  | Number
  | Print
  | PromptToggle
  | Quit
  | QuitForce
  | Read
  | Substitute
  | Transfer
  | NotGlobal
  | NotGlobalInteractive
  | Write
  | WriteAppend
  | Scroll
  | LineNumber
  | Goto

(** Return the string representation of a command *)
val string_of_command: t -> string

(** A module used to create new instances of Command.t *)
module Parser: sig
  (** The type of a command *)
  type unfinished

  (** An empty command *)
  val empty: unfinished

  (** Parses a line and returns a command *)
  val parse_line: unfinished -> string -> unfinished

  (** turns a unfinished command into a completed command *)
  val finish: unfinished -> t option
end
