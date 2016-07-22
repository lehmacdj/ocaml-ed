(*
 * This file is the entry point for oed and the main file for building the
 * binary. It parses the command line arguments and begins the main loop of the
 * program.
 *)

let version = "0.0.1"

let print_version () = print_endline version
let prompt = ref ""
let do_diagnostics = ref true

let main file =
  print_endline ("Editing " ^ file)
;;

type verbosity = Verbose | Suppressed | Normal

let verbosity = ref Normal
let will_encrypt = ref false
let prompt = ref ""

type arg =
  | S (* suppress output *)
  | X (* encrypt *)
  | P of string (* a prompt *)

(** split a string into characters in a list *)
let split str =
  let rec helper s acc =
    let l = String.length s in
    if l = 0 then acc
    else if l = 1 then (String.get s 0)::acc
    else helper (String.sub s 1 l) ((String.get s 0)::acc)
  in
  helper str []

(** Returns all of the arguments in a list *)
let get_args args =
  let get_arg = Re.compile @@ Re_perl.re "-(.*)" in
  let rec helper args acc =
    (** returns a list of args within short form args
     * e.g. -sx would return s and x *)
    let rec get_concise_args arg acc =
      failwith "fail"
    in
    failwith "fail"
  in
  failwith "hello"
;;
