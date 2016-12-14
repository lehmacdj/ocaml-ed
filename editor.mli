(*
 * Handles the processing of strings and the output of result strings.
 *)

open Core.Std
open Types

(** The type that represents a text editor. *)
type t

(** Create a new text editor in the input state. *)
val make: string -> t

(** Returns true if the editor is running. *)
val running: t -> bool

(** Execute a command on this editor and return the new state of the editor. *)
val execute: t -> command:EdCommand.t -> suffix:suffix -> t * output

(** Returns true if the editor is in verbose mode *)
val is_verbose: t -> bool
