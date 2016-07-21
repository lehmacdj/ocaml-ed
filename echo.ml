let rec get_address str =
  failwith "fail"
;;

let rec get_command str =
  failwith "fail"
;;

let rec get_args str =
  failwith "fail"
;;

let rec process_command command args =
  failwith "fail"
;;

let rec main =
  let will_exit = ref false in
  fun () ->
  if !will_exit then ()
  else
    let x = read_line () in
    let (address, remainder) = get_address x in
    let (command, remainder) = get_command remainder in
    let (args, remainder) = get_args remainder in
    let out =
      if String.length remainder > 0 then "invalid command suffix"
      else process_command command args in
    let () = print_endline out
    in
    main ()
;;

main ();;
