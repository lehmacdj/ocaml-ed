open Core.Std
open Re2.Std

(* we want to types to be accessible in scope of file *)
open Types

type t = command

let string_of_address = function
  | FirstLine -> "1"
  | Current -> "."
  | Offset n -> string_of_int n
  | OffsetFrom (l, n) ->
      (string_of_int l) ^
      (if n > 0 then "+" else "") ^
      (string_of_int n)
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
  | Delete range ->
      Printf.sprintf "%Sd"
          (string_of_address_range range)
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
  | Read (address, filename) ->
      Printf.sprintf "%Sr %S"
          (string_of_address address)
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
    Printf.printf "~lexed: [%S][%s][%S][%S][%S]\n"
        (Option.value ~default:"None" address_start)
        (Option.value ~default:"$"    address_separator)
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
    | None                              -> Some default
    | Some s when s = "1"               -> Some (FirstLine)
    | Some s when s = "."               -> Some (Current)
    | Some s when s = "$"               -> Some (LastLine)
    | Some s when Re2.matches num s     -> Some (Line (int_of_string s))
    | Some s when Re2.matches poffset s -> Some (Offset (-1))
    | Some s when Re2.matches noffset s -> Some (Offset (+1))
    | Some s                            -> None

  (** return an address based on an address string and a default address *)
  let parse_address = _parse_address ~relative_to:FirstLine

  (** return a pair of addresses representing an address range *)
  let parse_address_range addr1 delim addr2 ~default1 ~default2 =
    match addr1, delim, addr2 with
    | (a1, delim, a2) when delim = (Some ",") ->
        let addr1 = _parse_address a1 ~default:default1 ~relative_to:FirstLine in
        let addr2 = _parse_address a2 ~default:default2 ~relative_to:FirstLine in
        Option.both addr1 addr2
    | (a1, delim, a2) when delim = (Some ";") ->
        let addr1 = _parse_address a1 ~default:default1 ~relative_to:FirstLine in
        let addr2 = _parse_address a2 ~default:default2 ~relative_to:a1 in
        Option.both addr1 addr2
    | (_, Some _, _) -> None
    | (_, None, _)   -> None

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

    let addr_or_current = parse_address
        address_primary
        ~default:Current in

    let range_or_current = parse_address_range
        address_start
        address_separator
        address_primary
        ~default1:Current
        ~default2:Current in

    let range_or_buffer = parse_address_range
        address_start
        address_separator
        address_primary
        ~default1:FirstLine
        ~default2:LastLine in

    let invalid_address_error = Complete (ParseError "invalid address") in

    let filename = parse_filename args in

    (* match the command string to return the command type object *)
    match command with
    (* editing commands *)
    | "a" -> check_command_suffix args
        (match addr_or_current with
        | Some addr -> (Partial (Append (addr, [])))
        | None -> invalid_address_error)
    | "c" -> check_command_suffix args
        (match addr_or_current with
        | Some addr -> (Partial (Change (addr, [])))
        | None -> invalid_address_error)
    | "i" -> check_command_suffix args
        (match addr_or_current with
        | Some addr -> (Partial (Insert (addr, [])))
        | None -> invalid_address_error)
    | "d" ->
        (match range_or_current with
        | Some range -> (Complete (Delete range))
        | None -> invalid_address_error)
    | "j" -> check_command_suffix args
        (match range_or_current with
        | Some range -> (Complete (Join range))
        | None -> invalid_address_error)

    (* printing commands *)
    | "l" -> check_command_suffix args
        (match range_or_current with
        | Some range -> (Complete (List range))
        | None -> invalid_address_error)
    | "n" -> check_command_suffix args
        (match range_or_current with
        | Some range -> (Complete (Number range))
        | None -> invalid_address_error)
    | "p" -> check_command_suffix args
        (match range_or_current with
        | Some range -> (Complete (Print range))
        | None -> invalid_address_error)
    | "" -> check_command_suffix args
        (match addr_or_current with
        | Some addr -> (Complete (Goto addr))
        | None -> invalid_address_error)
    | "=" -> check_command_suffix args
        (match addr_or_current with
        | Some addr -> (Complete (LineNumber addr))
        | None -> invalid_address_error)


    (* file operations *)
    | "e" ->
        (match filename with
        | Ok filename -> Complete (Edit filename)
        | Error message -> Complete (ParseError message))
    | "f" ->
        (match filename with
        | Ok filename -> Complete (SetFile filename)
        | Error message -> Complete (ParseError message))

    (* write operations *)
    | "w" ->
        (match filename with
        | Ok filename ->
            (match range_or_buffer with
            | Some range -> Complete (Write (range, filename))
            | None -> invalid_address_error)
        | Error message -> Complete (ParseError message))
    | "W" ->
        (match filename with
        | Ok filename ->
            (match range_or_buffer with
            | Some range -> Complete (WriteAppend (range, filename))
            | None -> invalid_address_error)
        | Error message -> Complete (ParseError message))
    (* read *)
    | "r" ->
        (match filename with
        | Ok filename ->
            (match addr_or_current with
            | Some addr -> Complete (Read (addr, filename))
            | None -> invalid_address_error)
        | Error message -> Complete (ParseError message))

    (* 3 address *)
    | "m"
    | "t"

    (* help commands *)
    | "h" ->
        check_command_suffix args
        (Complete Help)
    | "H" -> check_command_suffix args
        (Complete HelpToggle)
    (* quit operations *)
    | "q" -> check_command_suffix args
        (Complete Quit)
    | "Q" -> check_command_suffix args
        (Complete QuitForce)
    (* toggle prompt *)
    | "P" -> check_command_suffix args
        (Complete PromptToggle)

    | "z" -> check_command_suffix args
        (* TODO: parse the line number to be correct *)
        (match addr_or_current with
        | Some addr -> Complete (Scroll (addr, 1))
        | None -> invalid_address_error)

    (* hard commands *)
    | "g"
    | "G"
    | "v"
    | "V"
    | "s" -> Complete (ParseError "Unimplemented command.")

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
      | Partial Global _ ->
          Complete (ParseError "Unimplimented command")
      | Partial ConverseGlobal _ ->
          Complete (ParseError "Unimplimented command")

      (* all of these things should never have to parse more than once *)
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
      | Complete _ -> failwith "this should never occur"

    let finish = function
      | Empty
      | Partial _ -> None
      | Complete c -> Some c
end
