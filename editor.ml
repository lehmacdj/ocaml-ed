(*
 * Implementation of editor.mli
 *)
open Core.Std
open Re2.Std
open Types
open Format

type command = EdCommand.t

type editor_response =
  | Nothing
  | UnspecifiedError
  | EdError of string
  | ByteCount of int

(*
 * The data the editor needs to store:
 *)
type t = {
  buffer: FileBuffer.t; (** The file and its contents *)
  undo: unit; (** Undo history. TODO: define this type *)
  verbose: bool; (** is the editor running in verbose mode *)
  error: string option; (** The output string *)
  line: int; (** The current line number *)
  running: bool; (** is the editor running *)
}

let make name =
  if name = "" then
    {
      buffer = FileBuffer.make None;
      undo = ();
      verbose = true; (* XXX: should start as false *)
      error = None;
      line = 1;
      running = true;
    }
  else
    {
      buffer = FileBuffer.make (Some name);
      undo = ();
      verbose = true; (* XXX: should start as false *)
      error = None;
      line = 1;
      running = true;
    }

let running editor = editor.running

let rec int_of_address editor = function
  | FirstLine -> 1
  | Current -> editor.line
  | Line n -> n
  | LastLine -> FileBuffer.lines editor.buffer
  | ForwardSearch re ->
      FileBuffer.find
        editor.buffer
        editor.line
        re
        ~direction:FileBuffer.Forward
  | BackwardSearch re ->
      FileBuffer.find
        editor.buffer
        editor.line
        re
        ~direction:FileBuffer.Backward
  | Offset (address, i) -> i + (int_of_address editor address)

(*
 * The default response for the editor. returns a editor_response that is either
 * nothing if there is no error, UnspecifiedError if the editor is not in
 * verbose mode and EdError editor.output if the editor is in verbose mode.
 *)
let default_response editor =
  match editor.error with
  | None                       -> Nothing
  | Some s when editor.verbose -> EdError s
  | Some _                     -> UnspecifiedError

(**
 * execute [command] on [editor] and return the new editor
 *)
let execute editor command =
  printf "~parsed: %s\n" @@ EdCommand.to_string command;
  print_flush ();
  match command with
  | Append (addr, text) ->
      let addr = int_of_address editor addr in
      ({ editor with
        buffer = FileBuffer.insert editor.buffer ~at:addr ~lines:text;
        line = addr;
        error = None;
        running = true;
      },
      Nothing)
  | Insert (addr, text) ->
      let addr = int_of_address editor addr in
      ({ editor with
        buffer = FileBuffer.insert editor.buffer ~at:(addr - 1) ~lines:text;
        line = addr;
        error = None;
      },
      Nothing)
  | Change ((addr1, addr2), text) ->
      let addr1 = int_of_address editor addr1 in
      let addr2 = int_of_address editor addr2 in
      ({ editor with
        buffer = editor.buffer
          |> FileBuffer.delete ~range:(addr1, addr2)
          |> FileBuffer.insert ~at:addr1 ~lines:text;
        line = addr1;
        error = None;
      },
      Nothing)
  | Delete (addr1, addr2) ->
      let addr1 = int_of_address editor addr1 in
      let addr2 = int_of_address editor addr2 in
      ({ editor with
        buffer = FileBuffer.delete ~range:(addr1, addr2) editor.buffer;
        line = addr1;
        error = None;
      },
      Nothing)
  | Help ->
      (editor, EdError (Option.value ~default:"no error" editor.error))
  | HelpToggle ->
      let editor = {editor with verbose = not editor.verbose} in
      (editor, default_response editor)
  | Edit name
  | EditForce name ->
      let editor = {editor with buffer = FileBuffer.set_name editor.buffer "test_name!"} in
      (editor, default_response editor)
  | Quit (* TODO: add state to make sure modifications are saved *)
  | QuitForce ->
      let editor = {editor with running = false} in
      (editor, Nothing)
  | _ ->
      ({editor with error = Some (EdCommand.to_string command)},
      default_response editor)

let response editor ~parse_error:error =
  if editor.verbose
  then EdError (EdParser.string_of_parse_error error)
  else UnspecifiedError
