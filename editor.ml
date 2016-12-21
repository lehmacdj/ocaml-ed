(*
 * Implementation of editor.mli
 * This really could use a facelift to a state Monad
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

(* in return consider factoring out the move and the resetting of the error
 * into separate functions *)
let append editor (addr, text) =
  let open Result.Monad_infix in
  int_of_address editor addr >>= fun addr ->
  FileBuffer.insert editor.buffer ~at:addr ~lines:text >>= fun buffer ->
  Result.return ({ editor with
    buffer = buffer;
    line = addr;
    error = None;
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

let print editor ~decorator (start, primary) =
  let open Result.Monad_infix in
  int_of_address editor start >>= fun start ->
  int_of_address editor primary >>= fun primary ->
  FileBuffer.lines editor.buffer ~range:(start, primary) >>= fun lines ->
  let decorated = List.mapi ~f:(fun i -> decorator (i + start)) lines in
  let text = String.concat ~sep:"\n" decorated in
  Result.return (editor, Text text)
;;

let set_file editor filename =
  let open Result.Monad_infix in
  match filename with
  | File name ->
      FileBuffer.set_name editor.buffer name >>= fun buffer ->
        Result.return ({editor with buffer = buffer}, PathName name)
  | ThisFile ->
      (match FileBuffer.name editor.buffer with
      | Some name -> Result.return (editor, PathName name)
      | None -> Result.return (editor, EdError (Some "no current filename")))
      | Command _ ->
          Result.return (editor, EdError (Some "invalid redirection"))
;;

let default_action editor command =
  Result.return ({editor with error = Some (EdCommand.to_string command)},
  default_response editor)
;;

let translate l ~assoc =
  List.bind l (fun e ->
    Option.value (List.Assoc.find assoc e) ~default:(List.return e))
;;

(* doesn't work because of simplictic way handling is done; something more
 * complex than insert and then delete is needed *)
let move editor (start, primary) target =
  let open Result.Monad_infix in
  int_of_address editor start >>= fun start ->
  int_of_address editor primary >>= fun primary ->
  int_of_address editor target >>= fun target ->
  FileBuffer.lines editor.buffer ~range:(start, primary) >>= fun lines ->
  FileBuffer.insert editor.buffer ~lines:lines ~at:target >>= fun buffer ->
  FileBuffer.delete buffer ~range:(start, primary) >>= fun buffer ->
  Result.return ({editor with buffer = buffer}, Nothing)
;;

let transfer editor (start, primary) target =
  let open Result.Monad_infix in
  int_of_address editor start >>= fun start ->
  int_of_address editor primary >>= fun primary ->
  int_of_address editor target >>= fun target ->
  FileBuffer.lines editor.buffer ~range:(start, primary) >>= fun lines ->
  FileBuffer.insert editor.buffer ~lines:lines ~at:target >>= fun buffer ->
  Result.return ({editor with buffer = buffer}, Nothing)
;;

(* The Characters defined in Table 5-1, Escape Sequences and Associated Actions:
                        (   '\\',    '\a',    '\b',    '\f',    '\r',    '\t',   '\v' ) *)
let list_chars        = [ '\012';  '\007';  '\010';  '\014';  '\000';  '\000';  '\000']
let list_char_strings = ["\\012"; "\\007"; "\\010"; "\\014"; "\\000"; "\\000"; "\\000"]
let list_map = List.zip_exn list_chars (List.map ~f:String.to_list list_char_strings)
;;

let execute editor ~command ~suffix =
  let open Result.Monad_infix in
  let return = function
    | Ok (e, msg) -> (e, msg)
    | Error be ->
        let editor = {editor with error =
          Some (FileBuffer.string_of_buffer_error be)} in
        (editor, default_response editor) in
  match command with
  | Append (addr, text) -> return @@ append editor (addr, text)
  | Insert (addr, text) -> return @@ insert editor (addr, text)
  | Change (range, text) -> return @@ change editor (range, text)
  | Delete range -> return @@ delete editor range
  (* | Join range -> return @@ join editor range *)
  | Move (range, target) -> return @@ move editor range target
  | Transfer (range, target) -> return @@ transfer editor range target
  | Help ->
      (editor, EdError editor.error)
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
  | Print range ->
      return @@ print editor
        ~decorator:Fn.(const id)
        range
  | List range ->
      let (&) = Fn.compose in
      return @@ print editor
        ~decorator:(const
          (String.of_char_list & translate ~assoc:list_map & String.to_list))
        range
  | Number range ->
      return @@ print editor
        ~decorator:(fun ln line -> (string_of_int ln) ^ "\t" ^ line)
        range
  | SetFile filename -> return @@ set_file editor filename

  | Join _
  | Global _
  | GlobalInteractive _
  | PromptToggle
  | Substitute _
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
