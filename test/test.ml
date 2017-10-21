open Batteries
module OU = OUnit2

let () =
  let open Monad in
  ()

let _ =
  OU.test_list
    [
      Msg_parse_tests.tests;
    ]
  |> OU.run_test_tt_main
