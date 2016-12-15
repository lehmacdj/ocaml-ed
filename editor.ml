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

let append editor (addr, text) =
  let open Result.Monad_infix in
  int_of_address editor addr >>= fun addr ->
  FileBuffer.insert editor.buffer ~at:addr ~lines:text >>= fun buffer ->
  Result.return ({ editor with
    buffer = buffer;
    line = addr;
    error = None;
    running = true;
  },
  Nothing)
;;

let insert editor (addr, text) =
  let open Result.Monad_infix in
  int_of_address editor addr >>= fun addr ->
  FileBuffer.insert editor.buffer ~at:(addr - 1) ~lines:text >>= fun buffer ->
  Result.return ({ editor with
    buffer = buffer;
    line = addr;
    error = None;
  },
  Nothing)
;;

let change editor ((start, primary), text) =
  let open Result.Monad_infix in
  int_of_address editor start >>= fun start ->
  int_of_address editor primary >>= fun primary ->
  FileBuffer.delete editor.buffer ~range:(start, primary) >>= fun buffer ->
  FileBuffer.insert buffer ~at:start ~lines:text >>= fun buffer ->
  Result.return ({ editor with
    buffer = buffer;
    line = start;
    error = None;
  },
  Nothing)
;;

let delete editor (start, primary) =
  let open Result.Monad_infix in
  int_of_address editor start >>= fun start ->
  int_of_address editor primary >>= fun primary ->
  FileBuffer.delete ~range:(start, primary) editor.buffer >>= fun buffer ->
  Result.return ({ editor with
    buffer = buffer;
    line = start;
    error = None;
  },
  Nothing)
;;

let print editor (start, primary) =
  let open Result.Monad_infix in
  int_of_address editor start >>= fun start ->
  int_of_address editor primary >>= fun primary ->
  FileBuffer.lines editor.buffer ~range:(start, primary) >>= fun buffer ->
  let text = List.fold_left ~init:"" ~f:(fun t e -> t ^ "\n" ^ e) buffer in
  Result.return (editor, Text text)
;;

let set_file editor filename =
  let open Result.Monad_infix in
  (match filename with
  | File name ->
      FileBuffer.set_name editor.buffer name >>= fun buffer ->
      Result.return ({editor with buffer = buffer}, PathName name)
  | ThisFile ->
      (match FileBuffer.name editor.buffer with
      | Some name -> Result.return (editor, PathName name)
      | None -> Result.return (editor, EdError (Some "no current filename")))
  | Command _ ->
      Result.return (editor, EdError (Some "invalid redirection")))
;;

let default_action editor command =
  Result.return ({editor with error = Some (EdCommand.to_string command)},
  default_response editor)
;;

let execute editor ~command ~suffix =
  let open Result.Monad_infix in
  let return = function
    | Ok (e, msg) -> (e, msg)
    | Error be ->
        let ed = {editor with error = Some (FileBuffer.string_of_buffer_error be)} in
        (ed, default_response editor) in
  match command with
  | Append (addr, text) -> return @@ append editor (addr, text)
  | Insert (addr, text) -> return @@ insert editor (addr, text)
  | Change ((start, primary), text) -> return @@ change editor ((start, primary), text)
  | Delete (start, primary) -> return @@ delete editor (start, primary)
  | Help -> (editor, EdError editor.error)
  | HelpToggle ->
      let editor = {editor with verbose = not editor.verbose} in
      (editor, default_response editor)
  | Edit filename (* TODO: logic for whether to editor or not *)
  | EditForce filename ->
      let file = (match filename with
          | File name -> FileBuffer.make (Some name)
          | Command _ -> failwith "editing commands is unimplimented"
          | ThisFile -> editor.buffer) in
      let editor = {editor with buffer = file} in
      (editor, default_response editor)
  | Quit (* TODO: add state to make sure modifications are saved *)
  | QuitForce ->
      let editor = {editor with running = false} in
      (editor, Nothing)
  | Print (start, primary) -> return @@ print editor (start, primary)
  | SetFile filename -> return @@ set_file editor filename

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
  | Goto _ -> return @@ default_action editor command
;;


let is_verbose editor = editor.verbose
;;
