open Batteries
module OU = OUnit2

let _ =
  OU.test_list
    [
      Msg_parse.Test.tests
    ]
  |> OU.run_test_tt_main
