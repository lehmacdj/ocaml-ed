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
  helper empty

(*
 * The main function of the program. Logic for input and output goes here along
 * with any necessary mutable (yuck) features.
 *)
let run filename =
  let () = Signal.Expert.handle Signal.int
    (fun _ ->
      (* TODO: not the actual behavior we want just a placeholder *)
      (* this works the arg of this function is the signal that was caught *)
      exit 0
    ) in
  let rec edit editor =
    let command = parse_command () in
    let editor = Editor.execute editor command in
    if Editor.verbose editor (* for now this will always be true *)
    then print_endline (Editor.out_string editor)
    else print_endline "?";
    edit editor in
  edit @@ Editor.make filename
