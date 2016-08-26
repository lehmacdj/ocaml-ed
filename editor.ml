(*
 * Implementation of editor.mli
 *)
open Core.Std
open Re2.Std

(*
 * The data the editor needs to store:
 *)
type t = {
  buffer: FileBuffer.t; (* The file and its contents *)
  undo: int; (* Undo history. FIXME: Shouldn't be an int *)
  verbose: bool; (* Whether or not in verbose mode *)
  output: string; (* The output string *)
  line: int; (* The current line number *)
}

(**
 * Fails the program with error "failure".
 *)
let fail () = failwith "failure"

let make name =
  if name = "" then
    {
      buffer = FileBuffer.make None;
      undo = 0;
      verbose = true;
      output = "blah";
      line = 1;
    }
  else
    {
      buffer = FileBuffer.make (Some name);
      undo = 0;
      verbose = true;
      output = "blah";
      line = 1;
    }

(**
 * Prints the output of an array.
 *)
let print_array =
  Array.iter ~f:(fun o -> print_string @@ (Option.value ~default:"None" o) ^ "|")

type command =
  | Append
  | Change
  | Delete
  | Insert

let string_of_command = function
  | Append -> "a"
  | Change -> "c"
  | Delete -> "d"
  | Insert -> "i"

(**
 * Dump the contents of an error variable to std::out
 *)
let error_dump e =
  failwith @@ "failed with error: " ^ (Error.to_string_mach e)

(**
 * Separate the addresses, command and args
 *)
let matches_from_command_line line =
  let address = "(\\+|\\-|\\d*|\\$|'[a-z])" in
  let command = "(a|c|d|i)" in
  let args = "(.*)" in
  let regex_str = ("^" ^ address ^ "(?:," ^ address ^ ")?" ^ command ^ args) in
  let regex = match Re2.create regex_str with
    | Ok re -> re
    | Error _ -> failwith ("Invalid regex: " ^ regex_str) in
  let matches = match Re2.find_submatches regex line with
    | Ok a -> Some a
    | Error _ -> None in
  matches

(**
 * return the command that corresponds with the string representing it
 *)
let get_command = function
  | "a" -> Append
  | "c" -> Change
  | "d" -> Delete
  | "i" -> Insert
  | _ -> failwith "Invalid command string"

(**
 * execute [command] with [range], [args] on [command]
 *)
let execute editor range command args =
  match command with
  | Append -> failwith "unimplemented"
  | Change -> failwith "unimplemented"
  | Delete -> failwith "unimplemented"
  | Insert -> failwith "unimplemented"

let process_string editor line =
  let matches = matches_from_command_line line in
  let () = match matches with
    | None -> print_endline "Could not parse command!"
    | Some m ->
        let range = AddressRange.make
          (Option.value ~default:"" (Array.get m 0))
          (Array.get m 1)
          editor.buffer in
        let command = get_command @@ Option.value ~default:"" @@ Array.get m 2 in
        let args = Option.value ~default:"" @@ Array.get m 3 in
        print_endline (
          "range: " ^
          (match range with None -> "None" | Some a -> AddressRange.to_string a)
          ^ ", " ^
          "command: " ^ (string_of_command command) ^ ", " ^
          "args: " ^ args
        )
  in
  editor

let verbose editor = editor.verbose

let out_string editor = editor.output
