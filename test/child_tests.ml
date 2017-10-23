open Batteries
open OUnit2

module Mock = struct
  exception PrematureQuit

  type logger = {
      mutable state : Child.state;
      outputs : (int, Text.t) Hashtbl.t;
    }

  module Monad = struct
    type 'a t = logger -> 'a
    let return = const
    let bind f g lo = g (f lo) lo
    let map g f lo = g (f lo)

    let con_id _ = 0
    let get_s lo = lo.state
    let put_s s lo = lo.state <- s

    let send i str lo =
      let add_str s = Text.append s (Text.of_string str) in
      Hashtbl.modify_def Text.empty i add_str lo.outputs

    let quit _ =
      raise PrematureQuit
  end
end


(** run mock with presupplied queue **)
let run_mock actions ~expect:expected =
  let module M = Mock.Monad in
  let module H = Child.Make(M) in
  (* init *)
  let outputs = Hashtbl.create 30 in
  let lo = {
      Mock.state = H.init ();
      Mock.outputs = outputs }
  in

  (* run all actions *)
  let remaining = ref (List.length actions) in
  (try
     actions
     |> List.iter (fun a ->
            remaining := !remaining - 1;
            match a with
            | `send s -> H.recv s lo)
   with
     Mock.PrematureQuit ->
      if !remaining > 0 then
        assert_failure "unexpected premature quit");

  (* check for expected substrings *)
  expected
  |> List.iter (function
         | `recv (i, substr) ->
            let txt = Hashtbl.find_default outputs i Text.empty in
            if Text.exists txt (Text.of_string substr) then
              assert_bool "ok" true
            else
              (let id = Random.int 100 in
               Printf.printf "\n[%d] BEGIN client output (con #%d)\n" id i;
               Printf.printf "%s" (Text.to_string txt);
               Printf.printf "\n[%d] END client output\n" id;
               assert_bool
                 (Printf.sprintf "[%d] missing substring %S" id substr)
                 false)

         | `final_state s ->
            assert_bool "final state not expected"
              (s = lo.Mock.state)
       )




let quit_test _ = begin
    run_mock [`send "QUIT\r\n"]
      ~expect:[`recv (0, "ERROR :Bye\r\n")];
  end


let err_test_1 _ = begin
    run_mock
      [`send "FOO\r\n"]
      [`recv (0, "421 * FOO :Unknown command\r\n")];

    run_mock
      [`send "NICK milo\r\n";
       `send "FOO\r\n"]
      [`recv (0, "421 milo FOO :Unknown command\r\n")];

  end

let err_test_2 _ = begin
    let param_count cmd n =
      let mk_args k = String.repeat " *" k in
      for i = 1 to n do
        run_mock [`send (cmd ^ mk_args (i - 1) ^ "\r\n")]
          ~expect:[`recv (0, cmd ^ " :Not enough parameters\r\n")]
      done
    in

    param_count "NICK" 1;
    param_count "USER" 4;
  end

let err_test_3 _ = begin
    run_mock
      [`send "NICK @a\r\n"]
      [`recv (0, "432 * @a :Erroneous nickname\r\n") ];
  end

let motd_test _ = begin
    run_mock
      [`send "NICK milo\r\n";
       `send "USER milo * * :Milo Turner\r\n"]
      [`recv (0, "375 :- test.irc Message of the day -\r\n");
       `recv (0, "372 :- Hello\r\n");
       `final_state (Child.LoggedIn "milo")];

    run_mock
      [`send "USER milo * * :Milo Turner\r\n";
       `send "NICK milo\r\n"]
      [`recv (0, "375 :- test.irc Message of the day -\r\n");
       `recv (0, "372 :- Hello\r\n");
       `final_state (Child.LoggedIn "milo")];

  end


let tests =
  "test.child"
  >::: List.rev [
      "quit" >:: quit_test;
      "err_1_unknown_cmd" >:: err_test_1;
      "err_2_param_count" >:: err_test_2;
      "err_3" >:: err_test_3;
      "motd" >:: motd_test;
    ]
