open Batteries
open OUnit2

let _ =
  run_test_tt_main
    (OUnit2.test_list
       [
         Msg_test.main;
         Actor_test.main;
       ])
