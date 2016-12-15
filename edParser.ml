open Core.Std
open Re2.Std
open Types

type command_state =
  | Empty (* a command on which parsing hasn't started yet *)
  | Partial of EdCommand.t * suffix (* an incomplete command *)
  | Complete of EdCommand.t * suffix (* a command that has a suffix *)
;;

type parse_error =
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

  (* XXX: DEBUG code only! *)
  (* print a debugging view for what was parsed *)
  Format.printf "~lexed: [%S][%s][%S][%S][%S]\n"
      (Option.value ~default:"None" address_start)
      (Option.value ~default:"$"    address_separator)
      (Option.value ~default:"None" address_primary)
      command
      args;
  Format.print_flush ();

  (address_start, address_separator, address_primary, command, args)
;;

(** a base function for parse address *)
let _parse_address address ~default ~relative_to:rel =
  let num = Re2.create_exn "\\d*" in
  (* TODO: make offsets work with arbitrary numbers and arbitrary primary addresses *)
  (* TODO: overhaul this more or less entirely *)
  let poffset = Re2.create_exn "[-^]" in
  let noffset = Re2.create_exn "\\+" in
  match address with
  | None                              -> Ok default
  | Some s when s = "1"               -> Ok FirstLine
  | Some s when s = "."               -> Ok rel
  | Some s when s = "$"               -> Ok LastLine
  | Some s when Re2.matches num s     -> Ok (Line (int_of_string s))
  | Some s when Re2.matches poffset s -> Ok (Offset (rel, -1))
  | Some s when Re2.matches noffset s -> Ok (Offset (rel, +1))
  | Some _                            -> Error InvalidAddress
;;

(** return an address based on an address string and a default address *)
let parse_address = _parse_address ~relative_to:FirstLine
;;

(** return a pair of addresses representing an address range *)
let parse_address_range start delim primary ~d_start ~d_primary =
  let open Result.Monad_infix in
  match delim with
  | Some "," ->
      _parse_address start ~default:d_start ~relative_to:Current >>= fun s ->
      _parse_address primary ~default:d_primary ~relative_to:Current >>= fun p ->
      Result.return (s, p)
  | Some ";" ->
      _parse_address start ~default:d_start ~relative_to:Current >>= fun s ->
      _parse_address primary ~default:d_primary ~relative_to:s >>= fun p ->
      Result.return (s, p)
  | None ->
      _parse_address primary ~default:d_primary ~relative_to:Current >>= fun p ->
      Result.return (p, p)
  | Some _ -> Error InvalidAddress
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
  let (start, separator, primary, command, args) = lex_first line in

  let suffix_completed command =
    if args <> ""
    (* return an error *)
    then Error InvalidCommandSuffix
    (* return the command and the suffix *)
    else Ok (Complete (command, NoSuffix)) in

  let suffix_partial command =
    if args <> ""
    (* return an error *)
    then Error InvalidCommandSuffix
    (* return the command and the suffix *)
    else Ok (Partial (command, NoSuffix)) in

  let addr_or_current = parse_address
      primary
      ~default:Current in

  let range_or_current = parse_address_range
      start
      separator
      primary
      ~d_start:Current
      ~d_primary:Current in

  let range_or_buffer = parse_address_range
      start
      separator
      primary
      ~d_start:FirstLine
      ~d_primary:LastLine in

  let filename = parse_filename args in

  (* match the command string to return the command type object *)
  let open Result.Monad_infix in
  match command with
  (* editing commands *)
  | "a" ->
      addr_or_current >>= (fun addr ->
      suffix_partial (Append (addr, [])))
  | "c" ->
      range_or_current >>= (fun range ->
      suffix_partial (Change (range, [])))
  | "i" ->
      addr_or_current >>= (fun addr ->
      suffix_partial (Insert (addr, [])))
  | "d" ->
      range_or_current >>= (fun range ->
      suffix_completed (Delete range))
  | "j" ->
      range_or_current >>= (fun range ->
      suffix_completed (Join range))

  (* printing commands *)
  (* FIXME: don't work for single numbers *)
  | "l" ->
      range_or_current >>= (fun range ->
      suffix_completed (List range))
  | "n" ->
      range_or_current >>= (fun range ->
      suffix_completed (Number range))
  | "p" ->
      range_or_current >>= (fun range ->
      suffix_completed (Print range))

  | "" ->
      addr_or_current >>= (fun addr ->
      suffix_completed (Goto addr))
  | "=" ->
      addr_or_current >>= (fun addr ->
      suffix_completed (LineNumber addr))

  (* file operations *)
  | "e" ->
      filename >>= (fun filename ->
      Ok (Complete (Edit filename, NoSuffix)))
  | "f" ->
      filename >>= (fun filename ->
      Ok (Complete (SetFile filename, NoSuffix)))

  (* write operations *)
  | "w" ->
      filename >>= (fun filename ->
      range_or_buffer >>= fun range ->
      Ok (Complete (Write (range, filename), NoSuffix)))
  | "W" ->
      filename >>= (fun filename ->
      range_or_buffer >>= fun range ->
      Ok (Complete (WriteAppend (range, filename), NoSuffix)))

  (* read *)
  | "r" ->
      filename >>= (fun filename ->
      addr_or_current >>= fun addr ->
      Ok (Complete (Read (addr, filename), NoSuffix)))

  (* 3 address *)
  | "m"
  | "t"

  (* help commands *)
  | "h" ->
      suffix_completed Help
  | "H" ->
      suffix_completed HelpToggle
  (* quit operations *)
  | "q" ->
      suffix_completed Quit
  | "Q" ->
      suffix_completed QuitForce
  (* toggle prompt *)
  | "P" ->
      suffix_completed PromptToggle

  | "z" ->
      (* TODO: parse the line number to be correct *)
      addr_or_current >>= fun addr ->
      suffix_completed (Scroll (addr, 1))

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
  state >>= function
    | Empty ->
        parse_first line
    | Partial (Append (a, lines), suf) ->
        if line = "."
        then Ok (Complete (Append (a, List.rev lines), suf))
        else Ok (Partial (Append (a, line :: lines), suf))
    | Partial (Change (r, lines), suf) ->
        if line = "."
        then Ok (Complete (Change (r, List.rev lines), suf))
        else Ok (Partial (Change (r, line :: lines), suf))
    | Partial (Insert (a, lines), suf) ->
        if line = "."
        then Ok (Complete (Insert (a, List.rev lines), suf))
        else Ok (Partial (Insert (a, line :: lines), suf))

    (* Parse the further global command *)
    | Partial (Global _, _) ->
        Error Unimplemented
    | Partial (ConverseGlobal _, _) ->
        Error Unimplemented

    (* all of these things should never have to parse more than once *)
    | Partial (Delete _, _)
    | Partial (Edit _, _)
    | Partial (EditForce _, _)
    | Partial (GlobalInteractive _, _)
    | Partial (Goto _, _)
    | Partial (Help, _)
    | Partial (HelpToggle, _)
    | Partial (Join _, _)
    | Partial (LineNumber _, _)
    | Partial (List _, _)
    | Partial (Move _, _)
    | Partial (ConverseGlobalInteractive _, _)
    | Partial (Number _, _)
    | Partial (Print _, _)
    | Partial (PromptToggle, _)
    | Partial (Quit, _)
    | Partial (QuitForce, _)
    | Partial (Read _, _)
    | Partial (Scroll _, _)
    | Partial (SetFile _, _)
    | Partial (Substitute _, _)
    | Partial (Transfer _, _)
    | Partial (Write _, _)
    | Partial (WriteAppend _, _)
    | Complete (_, _) -> failwith "tried to continue parsing when already finished"
;;

(* complete the parsing of a command returning an EdCommand or return None if
 * parsing has not yet terminated *)
let finish = function
  | Ok Empty
  | Ok Partial _ -> None
  | Ok Complete (c, s) -> Some (Ok (c, s))
  | Error e -> Some (Error e)
;;
