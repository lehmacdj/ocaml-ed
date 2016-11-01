(** Handles the parsing of all of the text passed into the text editor. *)

open Types

(** The type is a command *)
type t = command

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
