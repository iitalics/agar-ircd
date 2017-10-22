open Batteries
open Msg_parse
open OUnit2

let fmt = Printf.sprintf

(** assert that parsing 's' with 'p' produces expected result 'e' **)
let parses_to pp p s e =
  match C.parse p s with
  | Ok a -> assert_equal
              ~msg:(fmt "parsing %S" s)
              ~printer:pp
              e a
  | Bad x ->
     assert_failure (fmt "failed to parse %S" s)

(** assert that parsing 's' with 'p' fails **)
let parse_fails p s =
  match C.parse p s with
  | Ok a -> assert_failure (fmt "expected parse %S to fail" s)
  | Bad x -> assert_bool "ok" true


(**************************************************************************)


let prefix_test _ = begin
    let pp = function
      | Msg.Prefix_server s -> fmt "{server:%S}" s
      | Msg.Prefix_user (ni, us, ho) ->
         fmt "{nick:%S%s%s}"
           ni
           (Option.map_default (fmt ",user:%S") "" us)
           (Option.map_default (fmt ",host:%S") "" ho)
    in
    let pfx_srv s = Msg.Prefix_server s in
    let pfx_usr ni us ho = Msg.Prefix_user (ni, us, ho) in

    parses_to identity (hostname <<< P.eof)   "server.org"  "server.org";

    parses_to pp prefix    ":nick!usr@host.com " (pfx_usr "nick" (Some "usr") (Some "host.com"));
    parses_to pp prefix    ":nick!usr " (pfx_usr "nick" (Some "usr") None);
    parses_to pp prefix    ":nick " (pfx_usr "nick" None None);
    parses_to pp prefix    ":0.1.2.3 " (pfx_srv "0.1.2.3");
    parses_to pp prefix    ":123.1.2.64 " (pfx_srv "123.1.2.64");
    parses_to pp prefix    ":server.org " (pfx_srv "server.org");
    parse_fails prefix     ":";
    parse_fails prefix     ":abcfds\r\n";
  end

let params_test _ = begin
    let pp ss =
      String.concat ";" (List.map (fmt "%S") ss)
    in
    parses_to pp params    " a b c" ["a";"b";"c"];
    parses_to pp params    "" [];
    parses_to pp params    " a :b c" ["a";"b c"];
    parse_fails (params <<< P.eof) "a";
    parse_fails (params <<< P.eof) " a\nb";
  end

let message_test _ = begin
    let msg cmd pars =
      { Msg.raw_pfx = None;
        Msg.raw_cmd = cmd;
        Msg.raw_params = pars }
    in
    let msg_pfx_server srv cmd pars =
      { Msg.raw_pfx = Some (Msg.Prefix_server srv);
        Msg.raw_cmd = cmd;
        Msg.raw_params = pars }
    in
    let msg_pfx_nick nick cmd pars =
      { Msg.raw_pfx = Some (Msg.Prefix_user (nick, None, None));
        Msg.raw_cmd = cmd;
        Msg.raw_params = pars }
    in
    let pp m =
      let open Msg in
      fmt "%s%S %s"
        (match m.raw_pfx with
         | Some (Msg.Prefix_server x) -> fmt "{server:%S} " x
         | Some (Msg.Prefix_user (ni, us, ho)) -> fmt "{nick:%S} " ni
         | None -> "")
        m.raw_cmd
        (String.concat ";" (List.map (fmt "%S") m.raw_params))
    in

    parses_to pp raw_message     "QUIT\r\n" (msg "QUIT" []);
    parses_to pp raw_message     "PRIVMSG a b\r\n" (msg "PRIVMSG" ["a";"b"]);
    parses_to pp raw_message     "PRIVMSG a :b c\r\n" (msg "PRIVMSG" ["a";"b c"]);
    parses_to pp raw_message     "PRIVMSG a b :c d e\r\n" (msg "PRIVMSG" ["a";"b";"c d e"]);
    parses_to pp raw_message     "PRIVMSG :\r\n" (msg "PRIVMSG" [""]);
    parses_to pp raw_message     "PRIVMSG : :\r\n" (msg "PRIVMSG" [" :"]);
    parses_to pp raw_message     ":server.org 123\r\n" (msg_pfx_server "server.org" "123" []);
    parses_to pp raw_message     ":milo QUIT :eating lunch\r\n" (msg_pfx_nick "milo" "QUIT" ["eating lunch"]);

    parse_fails raw_message      "QUIT";
    parse_fails raw_message      "1234\r\n";
    parse_fails raw_message      "PRIVMSG a \rb\r\n";
    parse_fails raw_message      "PRIVMSG a\x00bc\r\n";
    parse_fails raw_message      "PRIVMSG a  b\r\n";
  end


(**************************************************************************)

let tests =
  "test.msg_parse"
  >::: [
      "prefix" >:: prefix_test;
      "params" >:: params_test;
      "message" >:: message_test;
    ]