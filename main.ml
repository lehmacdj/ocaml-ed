(*
 * Implemets main.mli.
 *)
open Core.Std

(**
 * Get a complete command from std::in
 *)
let parse_command () =
  let open EdCommand.Parser in
  let rec helper command =
    match finish command with
    | None -> helper (parse_line command (read_line ()))
    | Some c -> c in
  helper initial

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
    let command = parse_command () in
    let (editor, response) = E.execute editor command in
    (match response with
    | E.Nothing -> ()
    | E.UnspecifiedError -> print_endline "?"
    | E.ByteCount b -> print_endline (string_of_int b)
    | E.EdError m   -> print_endline m);
    if E.running editor
    then edit editor
    else () in
  edit @@ Editor.make filename
