open Core.Std
open Re2.Std
open Types
open Format

type command_state =
  | Empty
  | Partial of EdCommand.t
  | Complete of EdCommand.t
;;

type parse_error =
  | InvalidAddressRange
  | InvalidAddress
  | InvalidCommandSuffix
  | Unimplemented
  | InvalidFileName
;;

type t = (command_state, parse_error) Result.t
;;

(*
 * the initial parse state
 *)
let initial = Ok Empty
;;

let string_of_parse_error = function
  | InvalidAddressRange  -> "invalid address range"
  | InvalidAddress       -> "invalid address"
  | InvalidCommandSuffix -> "invalid command suffix"
  | Unimplemented        -> "unimplemented"
  | InvalidFileName      -> "invalid filename"
;;

let error_to_none = function
  | Ok x -> Some x
  | Error _ -> None
;;

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
  printf "~lexed: [%S][%s][%S][%S][%S]\n"
      (Option.value ~default:"None" address_start)
      (Option.value ~default:"$"    address_separator)
      (Option.value ~default:"None" address_primary)
      command
      args;
  print_flush ();

  (address_start, address_separator, address_primary, command, args)
;;

(** a base function for parse address *)
let _parse_address address ~default ~relative_to:_ =
  let num = Re2.create_exn "\\d*" in
  (* TODO: make offsets work with arbitrary numbers and arbitrary primary addresses *)
  let poffset = Re2.create_exn "[-^]" in
  let noffset = Re2.create_exn "\\+" in
  match address with
  | None                              -> Ok default
  | Some s when s = "1"               -> Ok (FirstLine)
  | Some s when s = "."               -> Ok (Current)
  | Some s when s = "$"               -> Ok (LastLine)
  | Some s when Re2.matches num s     -> Ok (Line (int_of_string s))
  | Some s when Re2.matches poffset s -> Ok (Offset (Current, -1))
  | Some s when Re2.matches noffset s -> Ok (Offset (Current, +1))
  | Some _                            -> Error InvalidAddress
;;

(** return an address based on an address string and a default address *)
let parse_address = _parse_address ~relative_to:FirstLine
;;

(** return a pair of addresses representing an address range *)
let parse_address_range addr1 delim addr2 ~default1 ~default2 =
  match addr1, delim, addr2 with
  | (a1, delim, a2) when delim = (Some ",") ->
      let addr1 = _parse_address a1 ~default:default1 ~relative_to:FirstLine in
      let addr2 = _parse_address a2 ~default:default2 ~relative_to:FirstLine in
      Option.value_map
          (Option.both (Result.ok addr1) (Result.ok addr2))
          ~default:(Error InvalidAddressRange)
          ~f:(fun v -> Ok v)
  | (a1, delim, a2) when delim = (Some ";") ->
      let addr1 = _parse_address a1 ~default:default1 ~relative_to:FirstLine in
      let addr2 = _parse_address a2 ~default:default2 ~relative_to:a1 in
      Option.value_map
          (Option.both (Result.ok addr1) (Result.ok addr2))
          ~default:(Error InvalidAddressRange)
          ~f:(fun v -> Ok v)
  | (_, Some _, _)
  | (_, None, _)   -> Error InvalidAddressRange
;;

(* returns the filename to be used based on [args] *)
let parse_filename args =
  let re = Re2.create_exn " (!)?(.*)" in
  match Re2.find_submatches re args with
  | Error _ -> Error InvalidCommandSuffix
  | Ok matches ->
      (match (Array.get matches 1, Array.get matches 2) with
      | None, None -> Ok (ThisFile)
      | None, Some name -> Ok (File name)
      | Some "!", Some name -> Ok (Command name)
      | Some _, None
      | Some _, Some _ -> Error InvalidFileName)
;;

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
  let validate_command_suffix command =
    if args <> ""
    then Error InvalidCommandSuffix
    else Ok command in

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

  let filename = parse_filename args in

  (* match the command string to return the command type object *)
  let open Result.Monad_infix in
  match command with
  (* editing commands *)
  | "a" ->
      addr_or_current >>= (fun addr ->
      validate_command_suffix (Partial (Append (addr, []))))
  | "c" ->
      range_or_current >>= (fun range ->
      validate_command_suffix (Partial (Change (range, []))))
  | "i" ->
      addr_or_current >>= (fun addr ->
      validate_command_suffix (Partial (Insert (addr, []))))
  | "d" ->
      range_or_current >>= (fun range ->
      validate_command_suffix (Complete (Delete range)))
  | "j" ->
      range_or_current >>= (fun range ->
      validate_command_suffix (Complete (Join range)))

  (* printing commands *)
  | "l" ->
      range_or_current >>= (fun range ->
      validate_command_suffix (Complete (List range)))
  | "n" ->
      range_or_current >>= (fun range ->
      validate_command_suffix (Complete (Number range)))
  | "p" ->
      range_or_current >>= (fun range ->
      validate_command_suffix (Complete (Print range)))
  | "" ->
      addr_or_current >>= (fun addr ->
      validate_command_suffix (Complete (Goto addr)))
  | "=" ->
      addr_or_current >>= (fun addr ->
      validate_command_suffix (Complete (LineNumber addr)))

  (* file operations *)
  | "e" ->
      filename >>= (fun filename ->
      Ok (Complete (Edit filename)))
  | "f" ->
      filename >>= (fun filename ->
      Ok (Complete (SetFile filename)))

  (* write operations *)
  | "w" ->
      filename >>= (fun filename ->
      range_or_buffer >>= fun range ->
      Ok (Complete (Write (range, filename))))
  | "W" ->
      filename >>= (fun filename ->
      range_or_buffer >>= fun range ->
      Ok (Complete (WriteAppend (range, filename))))

  (* read *)
  | "r" ->
      filename >>= (fun filename ->
      addr_or_current >>= fun addr ->
      Ok (Complete (Read (addr, filename))))

  (* 3 address *)
  | "m"
  | "t"

  (* help commands *)
  | "h" ->
      validate_command_suffix (Complete Help)
  | "H" ->
      validate_command_suffix (Complete HelpToggle)
  (* quit operations *)
  | "q" ->
      validate_command_suffix (Complete Quit)
  | "Q" ->
      validate_command_suffix (Complete QuitForce)
  (* toggle prompt *)
  | "P" ->
      validate_command_suffix (Complete PromptToggle)

  | "z" ->
      (* TODO: parse the line number to be correct *)
      addr_or_current >>= fun addr ->
      validate_command_suffix (Complete (Scroll (addr, 1)))

  (* hard commands *)
  | "g"
  | "G"
  | "v"
  | "V"
  | "s" -> Error Unimplemented

  | _ -> failwith "this should never occur"
;;

(**
 * finds the next state based on the previous state
 * - unstarted commands are parsed initially
 * - partial commands are updated to be closer to being complete
 * - complete commands are not changed
 *)
let parse_line state line =
  let open Result.Monad_infix in
  state >>= fun state ->
    match state with
    | Empty ->
        parse_first line
    | Partial Append (a, lines) ->
        if line = "."
        then Ok (Complete (Append (a, lines)))
        else Ok (Partial (Append (a, line :: lines)))
    | Partial Change (r, lines) ->
        if line = "."
        then Ok (Complete (Change (r, lines)))
        else Ok (Partial (Change (r, line :: lines)))
    | Partial Insert (a, lines) ->
        if line = "."
        then Ok (Complete (Insert (a, lines)))
        else Ok (Partial (Insert (a, line :: lines)))

        (* Parse the further global command *)
    | Partial Global _ ->
        Error Unimplemented
    | Partial ConverseGlobal _ ->
        Error Unimplemented

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
    | Complete _ -> failwith "this should never occur"
;;

(* complete the parsing of a command returning an EdCommand or return None if
 * parsing has not yet terminated *)
let finish = function
  | Ok Empty
  | Ok Partial _ -> None
  | Ok Complete c -> Some (Ok c)
  | Error e -> Some (Error e)
;;
