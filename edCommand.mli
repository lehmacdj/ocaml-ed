(** Handles the parsing of all of the text passed into the text editor. *)

open Core.Std
open Types

(** The type is a command *)
type t = command

(** Return the string representation of a command *)
val to_string: t -> string
