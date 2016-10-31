open EdCommand.Types
open OUnit2

let edit = Edit (File "hello")

let test1 test_ctxt = assert_equal edit (Edit (File "hello"))
let test2 test_ctxt = assert_equal 2 (1 + 1)

let suite =
  "suite" >::: [
    "test1" >:: test1;
    "test2" >:: test2;
  ]

let () = run_test_tt_main suite
