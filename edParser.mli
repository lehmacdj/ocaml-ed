(**
 * This module implements that allow the creation of EdCommand.t's by from
 * individual lines.
 *)

open Core.Std
open Types

(** the parse state *)
type t

(** an error while parsing *)
type parse_error

(** a string representation of the error *)
val string_of_parse_error: parse_error -> string

(** An empty command *)
val initial: t

(** Parses a line and returns the next parse state *)
val parse_line: t -> string -> t

(**
 * turns a unfinished command into a completed command. If the command has not
 * yet been completely parsed returns None.
 *)
val finish: t -> (EdCommand.t * suffix, parse_error) Result.t option
