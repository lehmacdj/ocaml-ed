(*
 * Implementation of buffer.mli
 *)
open Core.Std

(**
 * The name of the file (optional) and a list of all of the lines in the file.
 *)
type t = string option * string list

let make = function
  | None -> (None, [])
  | Some name ->
      (* Read the file and load it into the buffer *)
      match Sys.file_exists name with
      | `Yes ->
          failwith "no support for reading files yet"
      | `No ->
          (Some name, []) 
      | `Unknown ->
          failwith "no support for unknown file existence"

let get x =
  failwith "not implemented"

let write () =
  failwith "not implemented"
