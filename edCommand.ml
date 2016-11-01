open Core.Std
open Re2.Std

(* we want to types to be accessible in scope of file *)
open Types

type t = command

let string_of_address = function
  | FirstLine -> "1"
  | Current -> "."
  | Offset n -> string_of_int n
  | Line n -> string_of_int n
  | ForwardSearch re -> "/" ^ re ^ "/"
  | BackwardSearch re -> "?" ^ re ^ "?"
  | LastLine -> "$"

let string_of_filename = function
  | File name -> name
  | Command name -> "!" ^ name
  | ThisFile -> "ThisFile"

let string_of_address_range (address1, address2) =
  (string_of_address address1) ^ "," ^ (string_of_address address2)

let rec to_string = function
  | Append (address, lines) ->
      Printf.sprintf "%Sa\n    %s"
          (string_of_address address)
          (String.concat ~sep:"\n    " lines)
  | Change (address, lines) ->
      Printf.sprintf "%Sc\n    %s"
        (string_of_address address)
        (String.concat ~sep:"\n    " lines)
  | Delete address ->
      Printf.sprintf "%Sd"
          (string_of_address address)
  | Edit filename ->
      Printf.sprintf "e %S"
          (string_of_filename filename)
  | EditForce filename ->
      Printf.sprintf "E %S"
          (string_of_filename filename)
  | SetFile filename ->
      Printf.sprintf "f %S"
          (string_of_filename filename)
  | Global (address_range, regex, command) ->
      Printf.sprintf "%Sg/%S/%S"
          (string_of_address_range address_range)
          regex
          (to_string command)
  | GlobalInteractive (address_range, regex) ->
      Printf.sprintf "%SG/%S"
          (string_of_address_range address_range)
          regex
  | HelpToggle -> "H"
  | Help -> "h"
  | Insert (address, lines) ->
      Printf.sprintf "%Sa\n    %s"
          (string_of_address address)
          (String.concat ~sep:"\n    " lines)
  | Join (address_range) ->
      Printf.sprintf "%Sj"
          (string_of_address_range address_range)
  | List (address_range) ->
      Printf.sprintf "%Sl"
          (string_of_address_range address_range)
  | Move (address_range, address3) ->
      Printf.sprintf "%Sm%S"
          (string_of_address_range address_range)
          (string_of_address address3)
  | Number (address_range) ->
      Printf.sprintf "%Sn"
          (string_of_address_range address_range)
  | Print (address_range) ->
      Printf.sprintf "%Sp"
          (string_of_address_range address_range)
  | PromptToggle -> "P"
  | Quit -> "q"
  | QuitForce -> "Q"
  | Read filename ->
      Printf.sprintf "r %S"
          (string_of_filename filename)
  | Substitute (address_range, regex, substitution) ->
      Printf.sprintf "%Ss/%S/%S/"
          (string_of_address_range address_range)
          regex
          substitution
  | Transfer (address_range, address3) ->
      Printf.sprintf "%St%S"
          (string_of_address_range address_range)
          (string_of_address address3)
  | ConverseGlobal (address_range, regex, command) ->
      Printf.sprintf "%Sv/%S/%S"
          (string_of_address_range address_range)
          regex
          (to_string command)
  | ConverseGlobalInteractive (address_range, regex) ->
      Printf.sprintf "%SV/%S"
          (string_of_address_range address_range)
          regex
  | Write (address_range, filename) ->
      Printf.sprintf "%Sw %S"
          (string_of_address_range address_range)
          (string_of_filename filename)
  | WriteAppend (address_range, filename) ->
      Printf.sprintf "%SW %S"
          (string_of_address_range address_range)
          (string_of_filename filename)
  | Scroll (address, count) ->
      Printf.sprintf "%Sz%d"
          (string_of_address address)
          count
  | LineNumber address ->
      Printf.sprintf "%S="
          (string_of_address address)
  | Goto address ->
      Printf.sprintf "%S"
          (string_of_address address)
  | ParseError message ->
      Printf.sprintf "ParseError: %s"
          message

(** Used to create instances of command *)
module Parser = struct
  type parse_state =
    | Empty
    | Partial of t
    | Complete of t

  (*
   * the initial parse state
   *)
  let initial = Empty

  let error_to_none = function
    | Ok x -> Some x
    | Error _ -> None

  (** lex the first line of a command *)
  let lex_first line =
    (* address regex; TODO: add every kind of address to this *)
    let address_regex = "\\+|[-^]|\\d+|\\$|'[a-z]|;|," in
    (* all of the characters that denote the first character of a command *)
    let command_regex =
      "a|c|d|e|E|f|g|G|H|h|i|j|k|l|m|n|p|P|q|Q|r|s|t|u|v|w|W|z|=|" in
    (* match everything for arguments *)
    let args_regex    = ".*" in
    (* build the complete regex using ^ to anchor at start of string. *)
    let regex_str = ("^(?:(" ^ address_regex ^ ")(,|;))*"
                       ^ "(" ^ address_regex ^ ")?"
                       ^ "(" ^ command_regex ^ ")"
                       ^ "(" ^ args_regex    ^ ")") in
    let regex = Re2.create_exn regex_str in
    let matches = Re2.find_submatches regex line
      |> error_to_none
      |> Option.value ~default:(Array.create ~len:5 None) in

    (* get the necessary indices *)
    let address_start     = Array.get matches 1 in
    let address_separator = Array.get matches 2 in
    let address_primary   = Array.get matches 3 in
    let command           = Array.get matches 4 |> Option.value ~default:"" in
    let args              = Array.get matches 5 |> Option.value ~default:"" in

    (* print a debugging view for what was parsed *)
    Printf.printf "~parsed: [%S%s][%S][%S][%S]\n"
        (Option.value ~default:"None" address_start)
        (Option.value ~default:"$" address_separator)
        (Option.value ~default:"None" address_primary)
        command
        args;

    (address_start, address_separator, address_primary, command, args)

  (** a base function for parse address *)
  let _parse_address address ~default ~relative_to =
    let num = Re2.create_exn "\\d*" in
    let poffset = Re2.create_exn "[-^]" in
    let noffset = Re2.create_exn "\\+" in
    match address with
    | None -> default
    | Some s when s = "1"               -> FirstLine
    | Some s when s = "."               -> Current
    | Some s when s = "$"               -> LastLine
    | Some s when Re2.matches num s     -> Line (int_of_string s)
    | Some s when Re2.matches poffset s -> Offset (-1)
    | Some s when Re2.matches noffset s -> Offset (+1)
    | Some s                            -> failwith "cannot parse this address"

  (** return an address based on an address string and a default address *)
  let parse_address = _parse_address ~relative_to:FirstLine

  (** return a pair of addresses representing an address range *)
  let parse_address_range addr1 delim addr2 ~default1 ~default2 =
    match addr1, delim, addr2 with
    | (a1, delim, a2) when delim = (Some ",") ->
        (_parse_address a1 ~default:default1 ~relative_to:FirstLine,
        _parse_address a2 ~default:default2 ~relative_to:FirstLine)
    | (a1, delim, a2) when delim = (Some ";") ->
        let addr1 = _parse_address a1 ~default:default1 ~relative_to:FirstLine in
        (addr1, _parse_address a2 ~default:default2 ~relative_to:addr1)
    | (_, Some delim, _) -> failwith ("invalid delimiter " ^ delim)
    | (_, None, _) -> failwith ("no valid delimiter")

  (* returns the filename to be used based on [args] *)
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
        | Some _, Some _ -> Error "invalid file name")

  (**
   * Separate the addresses, command and args. Effectively the main parser of the
   * program. Some minor parsing occurs within each command in order to get the
   * arguments that that command requires.
   *)
  let parse_first line =
    let (address_start,
         address_separator,
         address_primary,
         command, args) = lex_first line in

    (* returns an error or [command] based on [args] *)
    let check_command_suffix args command =
      if args <> ""
      then Complete (ParseError "invalid command suffix")
      else command in

    let addr_or_current = parse_address ~default:Current in

    (* match the command string to return the command type object *)
    match command with
    | "a" -> check_command_suffix args
        (Partial (Append (addr_or_current address_primary, [])))
    | "c" -> check_command_suffix args
        (Partial (Change (addr_or_current address_primary, [])))
    | "i" -> check_command_suffix args
        (Partial (Insert (addr_or_current address_primary, [])))
    | "d" -> Complete (Delete (addr_or_current address_primary))

    (* filename operations *)
    | "e" ->
        (match parse_filename args with
        | Ok filename -> Complete (Edit filename)
        | Error message -> Complete (ParseError message))
    | "f" ->
        (match parse_filename args with
        | Ok filename -> Complete (SetFile filename)
        | Error message -> Complete (ParseError message))

    (* write operations *)
    | "w" ->
        (match parse_filename args with
        | Ok filename -> Complete (Write (
            (parse_address_range
                address_start
                address_separator
                address_primary
                ~default1:FirstLine
                ~default2:LastLine),
            filename))
        | Error message -> Complete (ParseError message))
    | "W" ->
        (match parse_filename args with
        | Ok filename -> Complete (WriteAppend (
            (parse_address_range
                address_start
                address_separator
                address_primary
                ~default1:FirstLine
                ~default2:LastLine),
            filename))
        | Error message -> Complete (ParseError message))

    | "g" -> failwith "unimplemented"
    | "G" -> failwith "unimplemented"
    | "H" -> failwith "unimplemented"
    | "h" -> failwith "unimplemented"
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
    | "z" -> failwith "unimplemented"
    | "=" -> failwith "unimplemented"
    | "" -> failwith "unimplemented"
    | x -> Complete (ParseError ("Invalid command string" ^ "\"" ^ x  ^ "\""))

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
          then Complete (Append (a, lines))
          else Partial (Append (a, line :: lines))
      | Partial Change (a, lines) ->
          if line = "."
          then Complete (Change (a, lines))
          else Partial (Change (a, line :: lines))
      | Partial Insert (a, lines) ->
          if line = "."
          then Complete (Insert (a, lines))
          else Partial (Change (a, line :: lines))

      (* Parse the further global command *)
      | Partial Global _ -> failwith "unimplemented"
      | Partial ConverseGlobal _ -> failwith "unimplemented"

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
      | Partial ConverseGlobalInteractive _
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
      | Complete _ -> failwith "should never occur"

    let finish = function
      | Empty
      | Partial _ -> None
      | Complete c -> Some c
end
