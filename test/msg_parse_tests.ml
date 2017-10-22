open Batteries
open Msg_parse
open OUnit2

let fmt = Printf.sprintf

(** assert that parsing 's' with 'p' produces expected result 'e' **)
let parses_to pp p s e =
  match C.parse (p <<< P.eof) s with
  | Ok a -> assert_equal
              ~msg:(fmt "parsing %S" s)
              ~printer:pp
              e a
  | Bad x ->
     assert_failure (fmt "failed to parse %S" s)

(** assert that parsing 's' with 'p' fails **)
let parse_fails p s =
  match C.parse (p <<< P.eof) s with
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

    parses_to pp prefix    ":0.1.2.3" (pfx_srv "0.1.2.3");
    parses_to pp prefix    ":123.1.2.64" (pfx_srv "123.1.2.64");
    parses_to pp prefix    ":server.org" (pfx_srv "server.org");
    parses_to pp prefix    ":nick" (pfx_usr "nick" None None);
    parses_to pp prefix    ":nick!usr" (pfx_usr "nick" (Some "usr") None);
    parses_to pp prefix    ":nick!usr@host.com" (pfx_usr "nick" (Some "usr") (Some "host.com"));
    parse_fails prefix     ":";
    parse_fails prefix     ":abc\rd";
    parse_fails prefix     ":abc\nd";
  end


let command_test _ = begin
    let pp = identity in
    parses_to pp command  "ABC" "ABC";
    parses_to pp command  "   ABC" "ABC";
    parses_to pp command  " PRIVMSG" "PRIVMSG";
    parses_to pp command  " 123" "123";
    parses_to pp command  " 685" "685";
    parse_fails command   " ";
    parse_fails command   " abc";
    parse_fails command   " AB4";
  end


let params_test _ = begin
    let pp ss =
      List.map (fmt "%S") ss
      |> String.concat ";"
      |> fmt "[%s]"
    in
    parses_to pp params    " a b c" ["a";"b";"c"];
    parses_to pp params    "" [];
    parses_to pp params    " a :b c" ["a";"b c"];
    parse_fails (params <<< P.eof) "a";
    parse_fails (params <<< P.eof) " a\nb";
  end


let message_test _ = begin
    let msg = Msg.simple in
    let msg_pfx_server srv cmd pars =
      Msg.with_prefix (Msg.Prefix_server srv) (msg cmd pars)
    in
    let msg_pfx_nick nick cmd pars =
      Msg.with_prefix (Msg.Prefix_user (nick, None, None)) (msg cmd pars)
    in
    let pp m =
      let open Msg in
      fmt "%s%S [%s]"
        (match m.raw_pfx with
         | Some (Msg.Prefix_server x) -> fmt "{server:%S} " x
         | Some (Msg.Prefix_user (ni, us, ho)) -> fmt "{nick:%S} " ni
         | None -> "")
        m.raw_cmd
        (String.concat ";" (List.map (fmt "%S") m.raw_params))
    in

    parses_to pp message     "QUIT\r\n" (msg "QUIT" []);
    parses_to pp message     "QUIT\r\n" (msg "QUIT" []);
    parses_to pp message     "PRIVMSG a b\r\n" (msg "PRIVMSG" ["a";"b"]);
    parses_to pp message     "PRIVMSG a   b\r\n" (msg "PRIVMSG" ["a";"b"]);
    parses_to pp message     "PRIVMSG a :b c\r\n" (msg "PRIVMSG" ["a";"b c"]);
    parses_to pp message     "PRIVMSG a b :c d e\r\n" (msg "PRIVMSG" ["a";"b";"c d e"]);
    parses_to pp message     "PRIVMSG :\r\n" (msg "PRIVMSG" [""]);
    parses_to pp message     "PRIVMSG : :\r\n" (msg "PRIVMSG" [" :"]);
    parses_to pp message     ":server.org 123\r\n" (msg_pfx_server "server.org" "123" []);
    parses_to pp message     ":k   PRIVMSG   a  :c \r\n" (msg_pfx_nick "k" "PRIVMSG" ["a";"c "]);
    parses_to pp message     ":milo QUIT :eating lunch\r\n" (msg_pfx_nick "milo" "QUIT" ["eating lunch"]);

    parse_fails message      "QUIT";
    parse_fails message      "1234\r\n";
    parse_fails message      "PRIVMSG a \rb\r\n";
    parse_fails message      "PRIVMSG a\x00bc\r\n";
  end


(**************************************************************************)

let tests =
  "test.msg_parse"
  >::: [
      "prefix" >:: prefix_test;
      "command" >:: command_test;
      "params" >:: params_test;
      "message" >:: message_test;
    ]
