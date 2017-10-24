
let () =
  OUnit2.run_test_tt_main
    (OUnit2.test_list
       [
         Msg_parse_tests.tests;
         Child_tests.tests;
         DB_tests.tests;
       ])
