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
          let file = In_channel.create name in
          let text = protect ~f:(fun () ->
              In_channel.input_lines file)
            ~finally:(fun () -> In_channel.close file) in
          (Some name, text)
      | `No ->
          (Some name, [])
      | `Unknown ->
          failwith @@ "File " ^ name ^ "is at an unknown location"

let get (_, text) line =
  List.nth text line

let write (name, text) =
  match name with
  | None ->
      failwith "filename is undefined"
  | Some name ->
      Out_channel.write_lines name text
