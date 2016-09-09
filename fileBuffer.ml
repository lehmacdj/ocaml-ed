(*
 * Implementation of buffer.mli
 *)
open Core.Std
open Re2.Std

(**
 * The name of the file (optional) and a list of all of the lines in the file,
 * and the number of lines in the file.
 *)
type t = string option * string list * int

let make = function
  | None -> (None, [], 0)
  | Some name ->
      (* Read the file and load it into the buffer *)
      match Sys.file_exists name with
      | `Yes ->
          let file = In_channel.create name in
          let text = protect ~f:(fun () ->
              In_channel.input_lines file)
            ~finally:(fun () -> In_channel.close file) in
          (Some name, text, 0)
      | `No ->
          (Some name, [], 0)
      | `Unknown ->
          failwith @@ "File " ^ name ^ "is at an unknown location"

let get (_, text, _) line =
  List.nth text line

let write (name, text, _) =
  match name with
  | None ->
      failwith "filename is undefined"
  | Some name ->
      Out_channel.write_lines name text

(* TODO: ensure that regex matching is the right regex matching for ed *)
let find (_, lines, count) current regex ?(direction = `Forward) =
  let re = match Re2.create @@ Re2.escape regex with
    | Ok re -> re
    | Error _ -> failwith ("Invalid regex while searching: " ^ regex) in
  let search_list = match direction with
    | `Forward -> List.drop lines (current - 1)
    | `Backward -> List.drop (List.rev lines) (count - current) in
  match List.findi search_list ~f:(fun _ -> Re2.matches re) with
  | None -> failwith "unimplemented"
  (* if not found go through the dropped elements of the list *)
  (* if not found in those then the search element is not in the list *)
  | Some (i, _) -> failwith "unimplemented"
