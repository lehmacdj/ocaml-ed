(*
 * Implemets main.mli.
 *)
open Core.Std

(*
 * The main function of the program. Logic for input and output goes here along
 * with any necessary mutable (yuck) features.
 *)
let run filename =
  let () = Signal.Expert.handle Signal.int
    (fun _ ->
      (* FIXME: not the actual behavior we want just a placeholder *)
      (* this works the arg of this function is the signal that was caught *)
      exit 0
    ) in
  let rec edit editor =
    let editor = Editor.process_string editor (read_line ()) in
    if Editor.verbose editor then print_endline @@ Editor.out_string editor
    else print_endline "?";
    edit editor in
  edit @@ Editor.make filename
