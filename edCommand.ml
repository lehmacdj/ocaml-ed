open Core.Std
open Re2.Std

module Types = struct
  type address = string
  type filename =
    (* either a file *)
    | File of string
    (* a command to read / write from *)
    | Command of string
    (* nothing; the current file *)
    | ThisFile
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
    | SetFile of filename
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
    | ParseError of string
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
      Printf.sprintf "e %S" (string_of_filename filename)
  | EditForce filename ->
      Printf.sprintf "E %S" (string_of_filename filename)
  | SetFile filename ->
      Printf.sprintf "f %S" (string_of_filename filename)
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
      Printf.sprintf "r %S" (string_of_filename filename)
  | Substitute (address1, address2, regex, substitution) ->
      Printf.sprintf "%S,%Ss/%S/%S/" address1 address2 regex substitution
  | Transfer (address1, address2, address3) ->
      Printf.sprintf "%S,%St%S" address1 address2 address3
  | NotGlobal (address1, address2, regex, command) ->
      Printf.sprintf "%S,%Sv/%S/%S" address1 address2 regex (to_string command)
  | NotGlobalInteractive (address1, address2, regex) ->
      Printf.sprintf "%S,%SV/%S" address1 address2 regex
  | Write (address1, address2, filename) ->
      Printf.sprintf "%S,%Sw %S" address1 address2 (string_of_filename filename)
  | WriteAppend (address1, address2, filename) ->
      Printf.sprintf "%S,%SW %S" address1 address2 (string_of_filename filename)
  | Scroll (address, count) ->
      Printf.sprintf "%Sz%d" address count
  | LineNumber address ->
      Printf.sprintf "%S=" address
  | Goto address ->
      Printf.sprintf "%S" address
  | ParseError message ->
      Printf.sprintf "ParseError: %s" message
and string_of_filename = function
  | File name -> name
  | Command name -> "!" ^ name
  | ThisFile -> "ThisFile"

(** Used to create instances of command *)
module Parser = struct
  type unfinished =
    | Empty
    | Partial of t
    | Completed of t

  (* an empty list represents a command that has not yet been completely parsed *)
  let empty = Empty

  let error_to_none = function
    | Ok x -> Some x
    | Error _ -> None

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
    let regex = Re2.create_exn regex_str in
    let matches = Re2.find_submatches regex line
      |> error_to_none
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

    let current_or_address = Option.value ~default:"." in

    (** returns an error or [command] based on [args] *)
    let check_command_suffix args command =
      if args <> ""
      then Completed (ParseError "invalid command suffix")
      else command in

    (** returns the filename to be used based on [args] *)
    let parse_filename args =
      let re = Re2.create_exn " (!)?(.*)" in
      match Re2.find_submatches re args with
      | Error _ -> Error "unexpected command suffix"
      | Ok matches ->
          (match (Array.get matches 1, Array.get matches 2) with
          | None, None -> Ok (ThisFile)
          | None, Some name -> Ok (File name)
          | Some "!", Some name -> Ok (Command name)
          | Some _, None
          | Some _, Some _ -> Error "invalid file name") in

    (** match the command string to return the command type object *)
    match command with
    | "a" -> check_command_suffix args
        (Partial (Append (current_or_address address_primary, [])))
    | "c" -> check_command_suffix args
        (Partial (Change (current_or_address address_primary, [])))
    | "d" -> Completed (Delete (current_or_address address_primary))
    | "e" ->
        (match parse_filename args with
        | Ok filename -> Completed (Edit filename)
        | Error message -> Completed (ParseError message))
    | "f" ->
        (match parse_filename args with
        | Ok filename -> Completed (SetFile filename)
        | Error message -> Completed (ParseError message))
    | "g" -> failwith "unimplemented"
    | "G" -> failwith "unimplemented"
    | "H" -> failwith "unimplemented"
    | "h" -> failwith "unimplemented"
    | "i" -> failwith "unimplemented"
    | "j" -> failwith "unimplemented"
    | "l" -> failwith "unimplemented"
    | "m" -> failwith "unimplemented"
    | "n" -> failwith "unimplemented"
    | "p" -> failwith "unimplemented"
    | "P" -> failwith "unimplemented"
    | "q" -> failwith "unimplemented"
    | "Q" -> failwith "unimplemented"
    | "r" -> failwith "unimplemented"
    | "s" -> failwith "unimplemented"
    | "t" -> failwith "unimplemented"
    | "v" -> failwith "unimplemented"
    | "V" -> failwith "unimplemented"
    | "w" -> failwith "unimplemented"
    | "W" -> failwith "unimplemented"
    | "z" -> failwith "unimplemented"
    | "=" -> failwith "unimplemented"
    | "" -> failwith "unimplemented"
    | x -> Completed (ParseError ("Invalid command string" ^ "\"" ^ x  ^ "\""))

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
      | Partial Append (a, lines) ->
          if line = "."
          then Completed (Append (a, lines))
          else Partial (Append (a, line :: lines))
      | Partial Change (a, lines) ->
          if line = "."
          then Completed (Change (a, lines))
          else Partial (Change (a, line :: lines))
      | Partial Insert (a, lines) ->
          if line = "."
          then Completed (Insert (a, lines))
          else Partial (Change (a, line :: lines))
      | Partial Global _ -> failwith "unimplemented"
      | Partial NotGlobal _ -> failwith "unimplemented"

      | Partial Delete _
      | Partial Edit _
      | Partial EditForce _
      | Partial GlobalInteractive _
      | Partial Goto _
      | Partial Help
      | Partial HelpToggle
      | Partial Join _
      | Partial LineNumber _
      | Partial List _
      | Partial Move _
      | Partial NotGlobalInteractive _
      | Partial Number _
      | Partial Print _
      | Partial PromptToggle
      | Partial Quit
      | Partial QuitForce
      | Partial Read _
      | Partial Scroll _
      | Partial SetFile _
      | Partial Substitute _
      | Partial Transfer _
      | Partial Write _
      | Partial WriteAppend _
      | Partial ParseError _
      | Completed _ -> failwith "should never occur"

    let finish = function
      | Empty
      | Partial _ -> None
      | Completed c -> Some c
end
