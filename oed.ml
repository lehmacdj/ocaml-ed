(*
 * This file is the entry point for oed and the main file for building the
 * binary. It parses the command line arguments and begins the main loop of the
 * program.
 * Currently no command line parsing is supported.
 *)
open Core.Std
open Format

(* TODO: parse command line args *)
(* TODO: put the name of the file to be edited here *)
(* TODO: decide where to put the handling of signals (probably in main.ml but it
 * could potentially be located here). *)

let () = Signal.Expert.handle Signal.int
  (fun _ ->
    (* TODO: not the actual behavior we want just a placeholder *)
    (* this works the arg of this function is the signal that was caught *)
    print_newline ();
    exit 0)

(**
 * Get a complete command from std::in
 *)
let parse_command () =
  let module P = EdParser in
  let rec helper command =
    match P.finish command with
    | None -> helper (P.parse_line command (read_line ()))
    | Some c -> c in
  helper P.initial

(* The core loop of the program; I should probably convert editor to be a Monad
 * But OCaml doesn't support do notation so it probably wouldn't be particularly
 * prettier than what we currently have. Eventually I will have to use Async
 * here in order to deal with the behavior we are supposed to do on <C-c> *)
let rec run editor =
  let module E = Editor in

  (* apply a command *)
  let (editor, response) =
    match parse_command () with
    | Ok (c, s) ->
        E.execute editor ~command:c ~suffix:s
    | Error e ->
        (editor, E.response editor ~parse_error:e) in

  (* output the necessary response *)
  (match response with
  | E.Nothing ->
      ()
  | E.UnspecifiedError ->
      printf "?\n";
      print_flush ()
  | E.ByteCount b ->
      printf "%d\n" b;
      print_flush ()
  | E.EdError m   ->
      printf "?\n%s\n" m;
      print_flush ()
  | E.Text t ->
      print_endline t;
      print_flush ()
  | E.PathName s ->
      printf "%s\n" s;
      print_flush ());

  (* determine whether or not to keep running *)
  if E.running editor
  then run editor
  else ()

let () = Command.run @@ Command.basic
  ~summary:"implementation of the posix standard ed line editor using OCaml"
  Command.Spec.(
    empty
    +> flag "-s" no_arg
      ~doc:" suppress output of bytecounts when reading and writing"
    +> flag "-p" (optional string)
      ~doc:"string prompt to display before executing commands"
    +> anon (maybe_with_default "" ("filename" %: string))
  )
  (fun _ _ filename () -> run @@ Editor.make filename)
