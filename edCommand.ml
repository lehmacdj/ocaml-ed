open Core.Std
open Re2.Std

module Types = struct
  type address = string
  type filename = string option
  type text = string list
  type regex = string

  (**
   * The first two addresses of any command are the address to execute the
   * command on. If there are more the remaining addresses are commands
   *)
  type command =
    | Append of address * text
    | Change of address * text
    | Delete of address
    | Edit of filename
    | EditForce of filename
    | File of filename
    | Global of address * address * regex * command
    | GlobalInteractive of address * address * regex
    | HelpToggle
    | Help
    | Insert of address * text
    | Join of address * address
    | List of address * address
    | Move of address * address * address
    | Number of address * address
    | Print of address * address
    | PromptToggle
    | Quit
    | QuitForce
    | Read of filename
    | Substitute of address * address * regex * string
    | Transfer of address * address * address
    | NotGlobal of address * address * regex * command
    | NotGlobalInteractive of address * address * regex
    | Write of address * address * filename
    | WriteAppend of address * address * filename
    | Scroll of address * int
    | LineNumber of address
    | Goto of address
end

(* we want to types to be accessible in scope of file *)
open Types

type t = command

let rec to_string = function
  | Append (address, lines) ->
      Printf.sprintf "%Sa\n    %s" address (String.concat ~sep:"\n    " lines)
  | Change (address, lines) ->
      Printf.sprintf "%Sc\n    %s" address (String.concat ~sep:"\n    " lines)
  | Delete address ->
      Printf.sprintf "%Sd" address
  | Edit filename ->
      Printf.sprintf "e %S" (opt_unwrap filename)
  | EditForce filename ->
      Printf.sprintf "E %S" (opt_unwrap filename)
  | File filename ->
      Printf.sprintf "f %S" (opt_unwrap filename)
  | Global (address1, address2, regex, command) ->
      Printf.sprintf "%S,%Sg/%S/%S" address1 address2 regex (to_string command)
  | GlobalInteractive (address1, address2, regex) ->
      Printf.sprintf "%S,%SG/%S" address1 address2 regex
  | HelpToggle -> "H"
  | Help -> "h"
  | Insert (address, lines) ->
      Printf.sprintf "%Sa\n    %s" address (String.concat ~sep:"\n    " lines)
  | Join (address1, address2) ->
      Printf.sprintf "%S,%Sj" address1 address2
  | List (address1, address2) ->
      Printf.sprintf "%S,%Sl" address1 address2
  | Move (address1, address2, address3) ->
      Printf.sprintf "%S,%Sm%S" address1 address2 address3
  | Number (address1, address2) ->
      Printf.sprintf "%S,%Sn" address1 address2
  | Print (address1, address2) ->
      Printf.sprintf "%S,%Sp" address1 address2
  | PromptToggle -> "P"
  | Quit -> "q"
  | QuitForce -> "Q"
  | Read filename ->
      Printf.sprintf "r %S" (opt_unwrap filename)
  | Substitute (address1, address2, regex, substitution) ->
      Printf.sprintf "%S,%Ss/%S/%S/" address1 address2 regex substitution
  | Transfer (address1, address2, address3) ->
      Printf.sprintf "%S,%St%S" address1 address2 address3
  | NotGlobal (address1, address2, regex, command) ->
      Printf.sprintf "%S,%Sv/%S/%S" address1 address2 regex (to_string command)
  | NotGlobalInteractive (address1, address2, regex) ->
      Printf.sprintf "%S,%SV/%S" address1 address2 regex
  | Write (address1, address2, filename) ->
      Printf.sprintf "%S,%Sw %S" address1 address2 (opt_unwrap filename)
  | WriteAppend (address1, address2, filename) ->
      Printf.sprintf "%S,%SW %S" address1 address2 (opt_unwrap filename)
  | Scroll (address, count) ->
      Printf.sprintf "%Sz%d" address count
  | LineNumber address ->
      Printf.sprintf "%S=" address
  | Goto address ->
      Printf.sprintf "%S" address
and opt_unwrap = Option.value  ~default:"None"

(** Used to create instances of command *)
module Parser = struct
  type unfinished =
    | Empty
    | Partial of t
    | Completed of t

  (* an empty list represents a command that has not yet been completely parsed *)
  let empty = Empty

  (** lex the first line of a command *)
  let lex_first line =
    let address_regex = "\\+|[-^]|\\d+|\\$|'[a-z]|;|," in
    let command_regex =
      "a|c|d|e|E|f|g|G|H|h|i|j|k|l|m|n|p|P|q|Q|r|s|t|u|v|w|W|z|=|" in
    let args_regex = ".*" in
    (* build the complete regex using ^ to anchor at start of string. *)
    let regex_str = ("^(?:(" ^ address_regex ^ ")(,|;))*"
        ^ "(" ^ address_regex ^ ")?"
        ^ "(" ^ command_regex ^ ")"
        ^ "(" ^ args_regex ^ ")") in
    let regex = match Re2.create regex_str with
      | Ok re -> re
      | Error _ -> failwith ("Invalid regex: " ^ regex_str) in
    let matches = (match Re2.find_submatches regex line with
      | Ok a -> Some a
      | Error _ -> None)
    |> Option.value ~default:(Array.create ~len:5 None) in
    let address_start = Array.get matches 1 in
    let address_separator = Array.get matches 2 in
    let address_primary = Array.get matches 3 in
    let command = Array.get matches 4 |> Option.value ~default:"" in
    let args = Array.get matches 5 |> Option.value ~default:"" in
    Printf.printf "~parsed: [%S%s][%S][%S][%S]\n"
        (Option.value ~default:"None" address_start)
        (Option.value ~default:"$" address_separator)
        (Option.value ~default:"None" address_primary)
        command
        args;
    (address_start, address_separator, address_primary, command, args)

  (**
   * Separate the addresses, command and args. Effectively the main parser of the
   * program. Some minor parsing occurs within each command in order to get the
   * arguments that that command requires.
   *)
  let parse_first line =
    let (address_start, address_separator, address_primary, command, args) =
      lex_first line in
    match command with
    | "a" ->
        if args <> ""
        then failwith "trailing arguments behind append"
        else Partial (Append (address_primary, []))
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
