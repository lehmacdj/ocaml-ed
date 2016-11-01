(*
 * Implementation of editor.mli
 *)
open Core.Std
open Re2.Std

type command = EdCommand.t

type editor_state =
  | ReadyForCommand (** execute a new command *)
  | ExecutingGlobalInteractive of unit (** TODO: keep track of this *)

(*
 * The data the editor needs to store:
 *)
type t = {
  buffer: FileBuffer.t; (** The file and its contents *)
  undo: unit; (** Undo history. TODO: define this type *)
  verbose: bool; (** is the editor running in verbose mode *)
  output: string; (** The output string *)
  line: int; (** The current line number *)
  state: editor_state; (** the state of the editor *)
}

let make name =
  if name = "" then
    {
      buffer = FileBuffer.make None;
      undo = ();
      verbose = false;
      output = "no error message";
      line = 1;
      state = ReadyForCommand;
    }
  else
    {
      buffer = FileBuffer.make (Some name);
      undo = ();
      verbose = false;
      output = "no error message";
      line = 1;
      state = ReadyForCommand;
    }

(**
 * execute [command] on [editor] and return the new editor
 *)
let execute editor command =
    {editor with output = EdCommand.to_string command}

let verbose editor = editor.verbose

let out_string editor = editor.output
