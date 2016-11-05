open Core.Std
open Re2.Std

(* we want to types to be accessible in scope of file *)
open Types

type t = command

let rec string_of_address = function
  | FirstLine -> "1"
  | Current -> "."
  | Offset (addr, n) ->
      (string_of_address addr) ^
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
      sprintf "%Sa\n    %s"
          (string_of_address address)
          (String.concat ~sep:"\n    " lines)
  | Change (range, lines) ->
      sprintf "%Sc\n    %s"
        (string_of_address_range range)
        (String.concat ~sep:"\n    " lines)
  | Delete range ->
      sprintf "%Sd"
          (string_of_address_range range)
  | Edit filename ->
      sprintf "e %S"
          (string_of_filename filename)
  | EditForce filename ->
      sprintf "E %S"
          (string_of_filename filename)
  | SetFile filename ->
      sprintf "f %S"
          (string_of_filename filename)
  | Global (address_range, regex, command) ->
      sprintf "%Sg/%S/%S"
          (string_of_address_range address_range)
          regex
          (to_string command)
  | GlobalInteractive (address_range, regex) ->
      sprintf "%SG/%S"
          (string_of_address_range address_range)
          regex
  | HelpToggle -> "H"
  | Help -> "h"
  | Insert (address, lines) ->
      sprintf "%Sa\n    %s"
          (string_of_address address)
          (String.concat ~sep:"\n    " lines)
  | Join (address_range) ->
      sprintf "%Sj"
          (string_of_address_range address_range)
  | List (address_range) ->
      sprintf "%Sl"
          (string_of_address_range address_range)
  | Move (address_range, address3) ->
      sprintf "%Sm%S"
          (string_of_address_range address_range)
          (string_of_address address3)
  | Number (address_range) ->
      sprintf "%Sn"
          (string_of_address_range address_range)
  | Print (address_range) ->
      sprintf "%Sp"
          (string_of_address_range address_range)
  | PromptToggle -> "P"
  | Quit -> "q"
  | QuitForce -> "Q"
  | Read (address, filename) ->
      sprintf "%Sr %S"
          (string_of_address address)
          (string_of_filename filename)
  | Substitute (address_range, regex, substitution) ->
      sprintf "%Ss/%S/%S/"
          (string_of_address_range address_range)
          regex
          substitution
  | Transfer (address_range, address3) ->
      sprintf "%St%S"
          (string_of_address_range address_range)
          (string_of_address address3)
  | ConverseGlobal (address_range, regex, command) ->
      sprintf "%Sv/%S/%S"
          (string_of_address_range address_range)
          regex
          (to_string command)
  | ConverseGlobalInteractive (address_range, regex) ->
      sprintf "%SV/%S"
          (string_of_address_range address_range)
          regex
  | Write (address_range, filename) ->
      sprintf "%Sw %S"
          (string_of_address_range address_range)
          (string_of_filename filename)
  | WriteAppend (address_range, filename) ->
      sprintf "%SW %S"
          (string_of_address_range address_range)
          (string_of_filename filename)
  | Scroll (address, count) ->
      sprintf "%Sz%d"
          (string_of_address address)
          count
  | LineNumber address ->
      sprintf "%S="
          (string_of_address address)
  | Goto address ->
      sprintf "%S"
          (string_of_address address)
