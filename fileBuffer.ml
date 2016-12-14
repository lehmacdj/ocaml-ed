(*
 * Implementation of buffer.mli
 *)
open Core.Std
open Re2.Std
open Types

type buffer_error =
  | InvalidAddress
  | NoMatchFound
;;

let string_of_file_buffer = function
  | InvalidAddress -> "invalid address"
  | NoMatchFound -> "no match"
;;

(**
 * The name of the file (optional) and a list of all of the lines in the file,
 * and the number of lines in the file.
 *)
type t = string option * string list * int
;;

let make = function
  | None -> (None, [], 0)
  | Some name ->
      (* Read the file and load it into the buffer *)
      match Sys.file_exists name with
      | `Yes ->
          let file = In_channel.create name in
          let text = protect
            ~f:(fun () -> In_channel.input_lines file)
            ~finally:(fun () -> In_channel.close file) in
          (Some name, text, 0)
      | `No ->
          (Some name, [], 0)
      | `Unknown ->
          failwith @@ "File " ^ name ^ "is at an unknown location"
;;

let set_name (_, text, count) name =
  (Some name, text, count)
;;

let name (name, _, _) =
  name
;;

(** asserts that n is inside the bounds of the buffer of [size] *)
let in_bounds n size =
  if n < size || n <= 0
  then
    (Format.printf "Expected n within [%d]" size;
    assert false)
;;

let get (_, text, _) line =
  List.nth text line
;;

let line_count (_, _, count) = count
;;

(* return the lines in range from buffer *)
let lines (_, text, size) ~range:(start, stop) =
  in_bounds start size;
  in_bounds stop size;
  List.filter_mapi text
    ~f:(fun i e ->
      if i >= start - 1 && i <= stop - 1
      then Some e
      else None)
;;

let write (name, text, _) ~range:_ =
  match name with
  | None ->
      failwith "filename is undefined"
  | Some name ->
      Out_channel.write_lines name text
;;

let delete (name, text, count) ~range:(start, stop) =
  let text = lines (name, text, count) ~range:(start, stop) in
  let count = count - stop + start in
  (name, text, count)
;;

let insert (name, text, count) ~at:index ~lines =
  let front = List.take text index in
  let back = List.drop text index in
  (name, front @ lines @ back, count)
;;

(**
 * Cycles a list. This is equivalent to making the element at indexed the first
 * element of a new list and appending all of the elements that were at the
 * start of the list to the end in the same order they initially were.
 *)
let cycle n lines =
  (List.drop lines n) @ (List.take lines n)
;;

(* the type for find *)
type search_direction =
  | Forward
  | Backward
;;

(* TODO: ensure that regex matching is the right regex matching for ed *)
let find (_, lines, count) current regex ~direction =
  let re = match Re2.create ~options:[] (Re2.escape regex) with
    | Ok re -> re
    | Error _ -> failwith ("Invalid regex while searching: " ^ regex) in
  let search_list = match direction with
    | Forward ->
        cycle current lines
    | Backward ->
        (* Line numbers are the line count - the line number when reversed *)
        cycle (count - current) (List.rev lines) in
  match List.findi search_list ~f:(fun _ -> Re2.matches re) with
  | None -> failwith "not found in the buffer"
  (* recompute the correct index for that line *)
  | Some (i, _) -> i + current
;;
