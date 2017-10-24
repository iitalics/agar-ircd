open Batteries
open OUnit2

module Tests_for(DB : Database.SIG) = struct

  let milo_info = {
      Database.user_name = "milo";
      Database.host_name = "0.1.2.3";
      Database.real_name = "Milo";
      Database.nick_name = "iiii";
      Database.modes = [ `o ];
    }

  let test1 _ =
    let db = DB.create_user_db () in
    begin
      assert_equal (DB.user_route "milo" db) None;
      assert_equal (DB.user_info "milo" db) None;
    end

  let test2 _ =
    let db = DB.create_user_db () in
    begin
      DB.add_user "milo" 4 None db;
      assert_equal (DB.user_route "milo" db) (Some 4);
      assert_equal (DB.user_info "milo" db) None;
    end

  let test3 _ =
    let db = DB.create_user_db () in
    begin
      DB.add_user "milo" 4 (Some milo_info) db;
      assert_equal (DB.user_route "milo" db) (Some 4);
      assert_equal (DB.user_info "milo" db) (Some milo_info);
    end

  let test4 _ =
    let db = DB.create_user_db () in
    begin
      DB.add_user "milo" 4 (Some milo_info) db;
      DB.add_user "ruby" 6 None db;
      assert_equal (DB.user_route "milo" db) (Some 4);
      assert_equal (DB.user_info "milo" db) (Some milo_info);
      assert_equal (DB.user_route "ruby" db) (Some 6);
      assert_equal (DB.user_info "ruby" db) None;
      assert_equal (DB.user_route "ema" db) None;
    end

  let test_list =
    [
      "test1" >:: test1;
      "test2" >:: test2;
      "test3" >:: test3;
      "test4" >:: test4;
    ]
end


let tests =
  let module T1 = Tests_for(Database.Hash_DB) in
  test_list
    [
      "test.db.hash" >::: T1.test_list
    ]
