open Batteries
open OUnit2
open Irc

let str_eq = assert_equal ~printer:(Printf.sprintf "%S")
let pfx_eq = assert_equal ~printer:(Printf.sprintf "%S" % Prefix.to_string)
let msg_eq = assert_equal ~printer:(Printf.sprintf "%S" % Msg.to_string)

let exn_eq e f =
  assert_equal ~printer:Printexc.to_string
    e
    (try f (); assert_failure "expected exception"
     with
     | Assert_failure x -> raise (Assert_failure x)
     | e' -> e')

let main =
  "Irc/Msg,Prefix" >:::
    [
      (*---------------*)
      "Msg/is_empty" >::
        begin fun _ ->
        assert_bool "empty is empty" @@ Prefix.is_empty Prefix.empty;
        assert_bool "non-empty nick" @@ not (Prefix.is_empty (Some "a", None, None));
        assert_bool "non-empty user" @@ not (Prefix.is_empty (None, Some "a", None));
        assert_bool "non-empty host" @@ not (Prefix.is_empty (None, None, Some "a"));
        end;

      (*---------------*)
      "Msg/simple,format" >::
        begin fun _ ->
        assert_equal "FOO" @@ (Msg.simple "FOO" ["x"]).command;
        assert_equal ["x"] @@ (Msg.simple "FOO" ["x"]).params;
        assert_equal ["a"; "b 3"] @@ (Msg.format "FOO" ["a"] "b %d" 3).params;
        end;

      (*---------------*)
      "Msg/to_string" >::
        begin fun _ ->
        str_eq "FOO x :y" @@
          Msg.to_string (Msg.simple "FOO" ["x"; "y"]);
        str_eq "FOO" @@
          Msg.to_string (Msg.simple "FOO" []);

        str_eq ":nick FOO" @@
          Msg.to_string
            { prefix = Prefix.of_nick "nick";
              command = "FOO";
              params = [] };

        str_eq ":nick@host BAR :a" @@
          Msg.to_string
            { prefix = Prefix.of_nick_host "nick" "host";
              command = "BAR";
              params = ["a"] };

        str_eq ":nick!user@host BAZ a b c d :e f g" @@
          Msg.to_string
            { prefix = Prefix.of_triple "nick" "user" "host";
              command = "BAZ";
              params = ["a"; "b"; "c"; "d"; "e f g"] };
        end;

      (*---------------*)
      "Msg/reply" >::
        begin fun _ ->
        msg_eq (Msg.simple "123" ["nick";"wow"]) (Msg.reply 123 ["wow"] "nick");
        msg_eq (Msg.simple "404" ["niiiick";"1+1=3"]) (Msg.replyf 404 [] "1+%d=3" 1 "niiiick");
        end;

      (*---------------*)
      "Prefix/of_string " >::
        begin fun _ ->
        pfx_eq (Prefix.of_triple "a" "b" "c") (Prefix.of_string "a!b@c");
        pfx_eq (Prefix.of_nick_host "a" "c") (Prefix.of_string "a@c");
        pfx_eq (Prefix.of_nick "a") (Prefix.of_string "a");
        end;

      (*---------------*)
      "Msg/of_string" >::
        begin fun _ ->
        msg_eq (Msg.simple "QUIT" [])
          (Msg.of_string "QUIT");
        msg_eq (Msg.simple "PRIVMSG" ["milo"; "hi"])
          (Msg.of_string "PRIVMSG milo hi");
        msg_eq (Msg.simple "PRIVMSG" ["milo"; "hi"])
          (Msg.of_string "     PRIVMSG   milo  hi     ");
        msg_eq (Msg.simple "PRIVMSG" ["milo"; "hi"; "world"])
          (Msg.of_string "PRIVMSG milo hi world");

        msg_eq (Msg.simple "PRIVMSG" ["milo"; "hi"])
          (Msg.of_string "PRIVMSG milo :hi");
        msg_eq (Msg.simple "PRIVMSG" ["milo"; "hi world"])
          (Msg.of_string "PRIVMSG milo :hi world");

        msg_eq ({ (Msg.simple "PRIVMSG" ["arisu"; "hi"])
                with Irc.prefix = Prefix.of_nick "lain" })
          (Msg.of_string ":lain PRIVMSG arisu :hi");

        msg_eq ({ (Msg.simple "PRIVMSG" ["arisu"; "hi"])
                with Irc.prefix = Prefix.of_nick_host "lain" "wired" })
          (Msg.of_string ":lain@wired   PRIVMSG arisu :hi");
        end;

      "Msg/of_string(errors)" >::
        begin fun _ ->
        exn_eq (Failure "Irc.Msg.of_string")
          (fun _ -> Msg.of_string "");
        exn_eq (Failure "Irc.Msg.of_string")
          (fun _ -> Msg.of_string "BAD_CMD");
        exn_eq (Failure "Irc.Msg.of_string")
          (fun _ -> Msg.of_string ":nick");
        exn_eq (Failure "Irc.Msg.of_string")
          (fun _ -> Msg.of_string ":nick :PRIVMSG");
        end;

    ]
