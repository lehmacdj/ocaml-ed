(*
 * Implementation of editor.mli
 *)
open Core.Std
open Re2.Std

(*
 * The editor needs to store the buffer in addition to undo information etc.
 *)
type t = Command.t * Buffer.t

let process_string line =
  let address = "\.|$|\d|[-^]*|[-^]\d|+*|+\d|[,%]|;|\/.*\/?|\?.*\??|'[a-z]" in
  let command = "" in
  let _ = Re2.create "" in
  failwith (address ^ command)
