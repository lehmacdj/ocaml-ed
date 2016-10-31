(** Handles the parsing of all of the text passed into the text editor. *)

(**
 * A set of type aliases common to several modules that originate in this
 * module
 *)
module Types : sig
  type address = string
  type filename =
    (* either a file *)
    | File of string
    (* a command to read / write from *)
    | Command of string
    (* nothing; the current file *)
    | ThisFile
  type text = string list
  type regex = string

  (**
   * The first two addresses of any command are the address to execute the
   * command on. If there are more the remaining addresses are commands
   *)
  type command =
    | Append of address * text
    | Change of address * text
    | Delete of address
    | Edit of filename
    | EditForce of filename
    | SetFile of filename
    | Global of address * address * regex * command
    | GlobalInteractive of address * address * regex
    | HelpToggle
    | Help
    | Insert of address * text
    | Join of address * address
    | List of address * address
    | Move of address * address * address
    | Number of address * address
    | Print of address * address
    | PromptToggle
    | Quit
    | QuitForce
    | Read of filename
    | Substitute of address * address * regex * string
    | Transfer of address * address * address
    | ConverseGlobal of address * address * regex * command
    | ConverseGlobalInteractive of address * address * regex
    | Write of address * address * filename
    | WriteAppend of address * address * filename
    | Scroll of address * int
    | LineNumber of address
    | Goto of address
    | ParseError of string
end

(** The type is a command *)
type t = Types.command

(** Return the string representation of a command *)
val to_string: t -> string

(** A module used to create new instances of Command.t *)
module Parser: sig
  (** The type of a command *)
  type parse_state

  (** An empty command *)
  val initial: parse_state

  (** Parses a line and returns a command *)
  val parse_line: parse_state -> string -> parse_state

  (** turns a unfinished command into a completed command *)
  val finish: parse_state -> t option
end
