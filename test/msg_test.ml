open Batteries
open OUnit2
open Irc

let s_eq = assert_equal ~printer:(Printf.sprintf "%S")
let pfx_eq = assert_equal ~printer:(Printf.sprintf "%S" % Prefix.to_string)
let msg_eq = assert_equal ~printer:(Printf.sprintf "%S" % Msg.to_string)

let _is_empty _ = begin
    assert_bool "empty is empty" @@ Prefix.is_empty Prefix.empty;
    assert_bool "non-empty nick" @@ not (Prefix.is_empty (Some "a", None, None));
    assert_bool "non-empty user" @@ not (Prefix.is_empty (None, Some "a", None));
    assert_bool "non-empty host" @@ not (Prefix.is_empty (None, None, Some "a"));
  end

let _msg_ctor _ = begin
    assert_equal "FOO" @@ (Msg.simple "FOO" ["x"]).command;
    assert_equal ["x"] @@ (Msg.simple "FOO" ["x"]).params;
    assert_equal ["a"; "b 3"] @@ (Msg.format "FOO" ["a"] "b %d" 3).params;
  end

let _msg_print _ = begin
    s_eq "FOO x :y" @@ Msg.to_string (Msg.simple "FOO" ["x"; "y"]);
    s_eq "FOO" @@ Msg.to_string (Msg.simple "FOO" []);
    s_eq ":host FOO" @@ Msg.to_string {
                            prefix = Prefix.of_host "host";
                            command = "FOO";
                            params = [] };

    s_eq ":user@host BAR :a" @@ Msg.to_string {
                                    prefix = Prefix.of_user_host "user" "host";
                                    command = "BAR";
                                    params = ["a"] };

    s_eq ":nick!user@host BAR :a" @@ Msg.to_string {
                                         prefix = Prefix.of_triple "nick" "user" "host";
                                         command = "BAR";
                                         params = ["a"] };
  end

let _replies _ = begin
    msg_eq (Msg.simple "123" ["nick";"wow"]) (Msg.reply 123 ["wow"] "nick");
    msg_eq (Msg.simple "404" ["niiiick";"1+1=3"]) (Msg.replyf 404 [] "1+%d=3" 1 "niiiick");
  end

let main =
  "irc_message/prefix" >:::
    [
      "is_empty" >:: _is_empty;
      "msg_ctor" >:: _msg_ctor;
      "msg_print" >:: _msg_print;
      "replies" >:: _replies;
    ]
