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

let make name =
  if name = "" then
    {
      buffer = FileBuffer.make None;
      undo = 0;
      verbose = true;
      output = "no error message";
      line = 1;
    }
  else
    {
      buffer = FileBuffer.make (Some name);
      undo = 0;
      verbose = true;
      output = "no error message";
      line = 1;
    }

(**
 * Prints the output of an array.
 *)
let print_matches x =
  Array.iter ~f:(fun o ->
    print_string @@ (Option.value ~default:"None" o) ^ "|") x;
  print_newline ()

type command =
  | Append
  | Change
  | Delete
  | Edit
  | File
  | Global
  | GlobalInteractive
  | HelpToggle
  | Help
  | Insert
  | Join
  | List
  | Move
  | Number
  | Print
  | PromptToggle
  | Quit
  | QuitForce
  | Read
  | Substitute
  | Transfer
  | NotGlobal
  | NotGlobalInteractive
  | Write
  | WriteAppend
  | Scroll
  | LineNumber
  | Goto

let string_of_command = function
  | Append -> "a"
  | Change -> "c"
  | Delete -> "d"
  | Edit -> "e"
  | File -> "f"
  | Global -> "g"
  | GlobalInteractive -> "G"
  | HelpToggle -> "H"
  | Help -> "h"
  | Insert -> "i"
  | Join -> "j"
  | List -> "l"
  | Move -> "m"
  | Number -> "n"
  | Print -> "p"
  | PromptToggle -> "P"
  | Quit -> "q"
  | QuitForce -> "Q"
  | Read -> "r"
  | Substitute -> "s"
  | Transfer -> "t"
  | NotGlobal -> "v"
  | NotGlobalInteractive -> "V"
  | Write -> "w"
  | WriteAppend -> "W"
  | Scroll -> "z"
  | LineNumber -> "="
  | Goto -> ""

(**
 * Dump the contents of an error variable to std::out
 *)
let error_dump e =
  failwith @@ "failed with error: " ^ (Error.to_string_mach e)

(**
 * Separate the addresses, command and args. Effectively the main parser of the
 * program. Some minor parsing occurs within each command in order to get the
 * arguments that that command requires.
 *)
let parse_string line =
  let address = "(\\+|[-^]|\\d+|\\$|'[a-z]|\\.|/.*/|\\?.*\\?)" in
  let command = "(a|c|d|e|E|f|g|G|H|h|i|j|k|l|m|n|p|P|q|Q|r|s|t|u|v|w|W|z|=|)" in
  let args = "(.*)" in
  (* build the complete regex using ^ to anchor at strat fo string. *)
  let regex_str = ("^" ^ address ^ "?(?:," ^ address ^ ")?" ^ command ^ args) in
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
let parse_command = function
  | "a" -> Append
  | "c" -> Change
  | "d" -> Delete
  | "e" -> Edit
  | "f" -> File
  | "g" -> Global
  | "G" -> GlobalInteractive
  | "H" -> HelpToggle
  | "h" -> Help
  | "i" -> Insert
  | "j" -> Join
  | "l" -> List
  | "m" -> Move
  | "n" -> Number
  | "p" -> Print
  | "P" -> PromptToggle
  | "q" -> Quit
  | "Q" -> QuitForce
  | "r" -> Read
  | "s" -> Substitute
  | "t" -> Transfer
  | "v" -> NotGlobal
  | "V" -> NotGlobalInteractive
  | "w" -> Write
  | "W" -> WriteAppend
  | "z" -> Scroll
  | "=" -> LineNumber
  | "" -> Goto
  | x -> failwith @@ "Invalid command string" ^ "\"" ^ x  ^ "\""

(**
 * parse a string that represents an address and return the correct line
 * number in the buffer for that address. [addr] is the address to parse and
 * [editor] is the current editor
 *)
let parse_address editor addr =
  failwith "unimplented"

(**
 * execute [command] with [range], [args] on [command]
 *)
let execute editor range command args =
  match command with
  | Append -> failwith "unimplemented"
  | Change -> failwith "unimplemented"
  | Delete -> failwith "unimplemented"
  | Edit -> failwith "unimplemented"
  | File -> failwith "unimplemented"
  | Global -> failwith "unimplemented"
  | GlobalInteractive -> failwith "unimplemented"
  | HelpToggle -> failwith "unimplemented"
  | Help -> failwith "unimplemented"
  | Insert -> failwith "unimplemented"
  | Join -> failwith "unimplemented"
  | List -> failwith "unimplemented"
  | Move -> failwith "unimplemented"
  | Number -> failwith "unimplemented"
  | Print -> failwith "unimplemented"
  | PromptToggle -> failwith "unimplemented"
  | Quit -> failwith "unimplemented"
  | QuitForce -> failwith "unimplemented"
  | Read -> failwith "unimplemented"
  | Substitute -> failwith "unimplemented"
  | Transfer -> failwith "unimplemented"
  | NotGlobal -> failwith "unimplemented"
  | NotGlobalInteractive -> failwith "unimplemented"
  | Write -> failwith "unimplemented"
  | WriteAppend -> failwith "unimplemented"
  | Scroll -> failwith "unimplemented"
  | LineNumber -> failwith "unimplemented"
  | Goto -> failwith "unimplemented"

let process_string editor line =
  let matches = parse_string line in
  let () = match matches with
    | None -> print_endline "Could not parse command!"
    | Some m ->
        print_matches m;
  in
  editor

let verbose editor = editor.verbose

let out_string editor = editor.output
