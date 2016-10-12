open EdCommand.Types
open OUnit2

let edit = Completed Edit File "hello"

let%test "edit" = edit = (Completed Edit File "hello")
