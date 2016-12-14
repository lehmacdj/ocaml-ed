(*
 * Implementation of editor.mli
 *)
open Core.Std
open Re2.Std
open Types

type command = EdCommand.t
;;

(* The data the editor needs to store: *)
type t = {
  buffer: FileBuffer.t; (** The file and its contents *)
  undo: unit; (** Undo history. TODO: define this type *)
  verbose: bool; (** is the editor running in verbose mode *)
  error: string option; (** The output string *)
  line: int; (** The current line number *)
  running: bool; (** is the editor running *)
}
;;

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
;;

let running editor = editor.running
;;

let rec int_of_address editor =
  let open Result.Monad_infix in function
  | FirstLine -> Result.return 1
  | Current -> Result.return editor.line
  | Line n -> Result.return n
  | LastLine -> Result.return @@ FileBuffer.line_count editor.buffer
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
  | Offset (address, i) ->
      int_of_address editor address >>= fun address_value ->
      Result.return @@ i + address_value
;;

(*
 * The default response for the editor. returns a editor_response that is either
 * nothing if there is no error, UnspecifiedError if the editor is not in
 * verbose mode and EdError editor.output if the editor is in verbose mode.
 *)
let default_response editor =
  match editor.error with
  | None                       -> Nothing
  | Some s when editor.verbose -> EdError (Some s)
  | Some _                     -> EdError None
;;

let execute editor ~command ~suffix =
  let open Result.Monad_infix in
  match command with
  | Append (addr, text) ->
      let addr = int_of_address editor addr in
      FileBuffer.insert editor.buffer ~at:addr ~lines:text >>= fun buf ->
      ({ editor with
        buffer = buffer;
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
  | Change ((start, primary), text) ->
      let start = int_of_address editor start in
      let primary = int_of_address editor primary in
      ({ editor with
        buffer = editor.buffer
          |> FileBuffer.delete ~range:(start, primary)
          |> FileBuffer.insert ~at:start ~lines:text;
        line = start;
        error = None;
      },
      Nothing)
  | Delete (start, primary) ->
      let start = int_of_address editor start in
      let primary = int_of_address editor primary in
      ({ editor with
        buffer = FileBuffer.delete ~range:(start, primary) editor.buffer;
        line = start;
        error = None;
      },
      Nothing)
  | Help ->
      (editor, EdError editor.error)
  | HelpToggle ->
      let editor = {editor with verbose = not editor.verbose} in
      (editor, default_response editor)
  | Edit filename (* TODO: logic for whether to editor or not *)
  | EditForce filename ->
      let file = (match filename with
          | File name -> FileBuffer.make (Some name)
          | Command _ -> failwith "failure"
          | ThisFile -> editor.buffer) in
      let editor = {editor with buffer = file} in
      (editor, default_response editor)
  | Quit (* TODO: add state to make sure modifications are saved *)
  | QuitForce ->
      let editor = {editor with running = false} in
      (editor, Nothing)
  | Print (start, primary) ->
      let start = int_of_address editor start in
      let primary = int_of_address editor primary in
      let text = List.fold_left
        ~init:""
        ~f:(fun t e -> t ^ "\n" ^ e)
        (FileBuffer.lines editor.buffer
          ~range:(start, primary)) in
      (editor, Text text)
  | SetFile filename ->
      (match filename with
      | File name ->
          ({editor with buffer = FileBuffer.set_name editor.buffer name},
          PathName name)
      | ThisFile ->
          (match FileBuffer.name editor.buffer with
          | Some name -> (editor, PathName name)
          | None -> (editor, EdError (Some "no current filename")))
      | Command _ ->
          (editor, EdError (Some "invalid redirection")))

  | Global _
  | GlobalInteractive _
  | Join _
  | List _
  | Move _
  | Number _
  | PromptToggle
  | Substitute _
  | Transfer _
  | ConverseGlobal _
  | ConverseGlobalInteractive _
  | Write _
  | WriteAppend _
  | Scroll _
  | LineNumber _
  | Read _
  | Goto _ ->
      ({editor with error = Some (EdCommand.to_string command)},
      default_response editor)
;;

let is_verbose editor = editor.verbose
;;
