open Batteries
module OU = OUnit2


let _ =
  OU.run_test_tt_main
    (OU.test_list
       [ Msg_parse.Test.tests
       ])
