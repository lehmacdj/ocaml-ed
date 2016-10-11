(*
 * Implementation of editor.mli
 *)
open Core.Std
open Re2.Std

(*
 * The data the editor needs to store:
 *)
type t = {
  buffer: FileBuffer.t; (* The file and its contents *)
  undo: int; (* Undo history. FIXME: Shouldn't be an int *)
  verbose: bool; (* Whether or not in verbose mode *)
  output: string; (* The output string *)
  line: int; (* The current line number *)
}

let make name =
  if name = "" then
    {
      buffer = FileBuffer.make None;
      undo = 0;
      verbose = true;
      output = "no error message";
      line = 1;
    }
  else
    {
      buffer = FileBuffer.make (Some name);
      undo = 0;
      verbose = true;
      output = "no error message";
      line = 1;
    }

(**
 * execute [command] on [editor] and return the new editor
 *)
let execute editor command =
    {editor with output = EdCommand.string_of_command command}

let verbose editor = editor.verbose

let out_string editor = editor.output
