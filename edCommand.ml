open Core.Std
open Re2.Std

type t =
  | Append of string list
  | Change of string list
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
  | Append l -> "a of " ^ (String.concat ~sep:"\n    " l)
  | Change l -> "c of " ^ (String.concat ~sep:"\n    " l)
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

(** Used to create instances of command *)
module Parser = struct
  type unfinished =
    | Empty
    | Partial of t
    | Completed of t

  (* an empty list represents a command that has not yet been completely parsed *)
  let empty = Empty

  (**
   * Separate the addresses, command and args. Effectively the main parser of the
   * program. Some minor parsing occurs within each command in order to get the
   * arguments that that command requires.
   *)
   let parse_first line =
    let address_regex = "(\\+|[-^]|\\d+|\\$|'[a-z])" in
    let command_regex =
      "(a|c|d|e|E|f|g|G|H|h|i|j|k|l|m|n|p|P|q|Q|r|s|t|u|v|w|W|z|=|)" in
    let args_regex = "(.*)" in
    (* build the complete regex using ^ to anchor at start of string. *)
    let regex_str = ("^" ^ address_regex
        ^ "?(?:," ^ address_regex ^ ")?"
        ^ command_regex
        ^ args_regex) in
    let regex = match Re2.create regex_str with
      | Ok re -> re
      | Error _ -> failwith ("Invalid regex: " ^ regex_str) in
    let matches = match Re2.find_submatches regex line with
      | Ok a -> Some a
      | Error _ -> None in
    let sanitized = Option.value_map ~default:(Array.create ~len:5 "")
        ~f:(Array.map ~f:(Option.value ~default:"")) matches in
    let address1 = Array.get sanitized 1 in
    let address2 = Array.get sanitized 2 in
    let command = Array.get sanitized 3 in
    let args = Array.get sanitized 4 in
    Printf.printf "~parsed: [%S][,%S][%S][%S]\n" address1 address2 command args;
    match command with
    | "a" ->
        if args <> ""
        then failwith "trailing arguments behind append"
        else Partial (Append [])
    | "c" ->
        if args <> ""
        then failwith "trailing arguments behind change"
        else Partial (Change [])
    | "d" ->
        if args <> ""
        then failwith "trailing arguments behind delete"
        else Completed Delete
    | "e" -> Completed Edit
    | "f" -> Completed File
    | "g" -> Completed Global
    | "G" -> Completed GlobalInteractive
    | "H" -> Completed HelpToggle
    | "h" -> Completed Help
    | "i" -> Completed Insert
    | "j" -> Completed Join
    | "l" -> Completed List
    | "m" -> Completed Move
    | "n" -> Completed Number
    | "p" -> Completed Print
    | "P" -> Completed PromptToggle
    | "q" -> Completed Quit
    | "Q" -> Completed QuitForce
    | "r" -> Completed Read
    | "s" -> Completed Substitute
    | "t" -> Completed Transfer
    | "v" -> Completed NotGlobal
    | "V" -> Completed NotGlobalInteractive
    | "w" -> Completed Write
    | "W" -> Completed WriteAppend
    | "z" -> Completed Scroll
    | "=" -> Completed LineNumber
    | "" -> Completed Goto
    | x -> failwith @@ "Invalid command string" ^ "\"" ^ x  ^ "\""

  (**
   * finds the next state based on the previous state
   * - unstarted commands are parsed initially
   * - partial commands are updated to be closer to being complete
   * - complete commands are not changed
   *)
  let parse_line state line =
    match state with
    | Empty ->
        parse_first line
    | Partial Append lines ->
        if line = "."
        then Completed (Append lines)
        else Partial ((Append (line :: lines)))
    | Partial Change lines ->
        if line = "."
        then Completed (Append lines)
        else Partial ((Append (line :: lines)))
    | Partial Delete -> failwith "failure"
    | Partial Edit -> failwith "failure"
    | Partial File -> failwith "failure"
    | Partial Global -> failwith "failure"
    | Partial GlobalInteractive -> failwith "failure"
    | Partial HelpToggle -> failwith "failure"
    | Partial Help -> failwith "failure"
    | Partial Insert -> failwith "failure"
    | Partial Join -> failwith "failure"
    | Partial List -> failwith "failure"
    | Partial Move -> failwith "failure"
    | Partial Number -> failwith "failure"
    | Partial Print -> failwith "failure"
    | Partial PromptToggle -> failwith "failure"
    | Partial Quit -> failwith "failure"
    | Partial QuitForce -> failwith "failure"
    | Partial Read -> failwith "failure"
    | Partial Substitute -> failwith "failure"
    | Partial Transfer -> failwith "failure"
    | Partial NotGlobal -> failwith "failure"
    | Partial NotGlobalInteractive -> failwith "failure"
    | Partial Write -> failwith "failure"
    | Partial WriteAppend -> failwith "failure"
    | Partial Scroll -> failwith "failure"
    | Partial LineNumber -> failwith "failure"
    | Partial Goto -> failwith "failure"
    | Completed c -> Completed c

  let finish = function
    | Empty
    | Partial _ -> None
    | Completed c -> Some c
end
