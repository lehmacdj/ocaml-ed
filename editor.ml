(*
 * Implementation of editor.mli
 *)
open Core.Std
open Re2.Std
open Types

type command = EdCommand.t

type editor_state =
  | ReadyForCommand (** execute a new command *)
  | ExecutingGlobalInteractive of unit (** TODO: keep track of this *)


type editor_response =
  | Nothing
  | UnknownError
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
  state: editor_state; (** the state of the editor *)
}

let make name =
  if name = "" then
    {
      buffer = FileBuffer.make None;
      undo = ();
      verbose = false;
      error = None;
      line = 1;
      state = ReadyForCommand;
    }
  else
    {
      buffer = FileBuffer.make (Some name);
      undo = ();
      verbose = false;
      error = None;
      line = 1;
      state = ReadyForCommand;
    }

(*
 * The default response for the editor. returns a editor_response that is either
 * nothing if there is no error, UnknownError if the editor is not in verbose
 * mode and EdError editor.output if the editor is in verbose mode.
 *)
let default_response editor =
  match editor.error with
  | None                       -> Nothing
  | Some s when editor.verbose -> EdError s
  | Some _                     -> UnknownError

(**
 * execute [command] on [editor] and return the new editor
 *)
let execute editor command =
  match command with
  | Help ->
      (editor,
      EdError (Option.value ~default:"invalid address" editor.error))
  | HelpToggle ->
      ({editor with verbose = not editor.verbose},
      match editor.error with
      | None -> Nothing
      | Some s -> EdError s)
  | _ ->
      ({editor with error = Some (EdCommand.to_string command)},
      default_response editor)
