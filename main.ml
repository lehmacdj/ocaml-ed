(*
 * Implemets main.mli.
 *)
open Core.Std
open Format

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

(*
 * The main function of the program. Logic for input and output goes here along
 * with any necessary mutable (yuck) features.
 *)
let run filename =
  let () = Signal.Expert.handle Signal.int
    (fun _ ->
      (* TODO: not the actual behavior we want just a placeholder *)
      (* this works the arg of this function is the signal that was caught *)
      print_newline ();
      exit 0
    ) in

  let rec edit editor =
    let module E = Editor in

    (* apply a command *)
    let (editor, response) =
      match parse_command () with
      | Ok command ->
          E.execute editor command
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
        print_flush ());

    (* determine whether or not to keep running *)
    if E.running editor
    then edit editor
    else () in

  edit @@ Editor.make filename
