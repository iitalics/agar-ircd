open Batteries
module P = ParserCo
module C = CharParser
open ParserCo.Infix


(* extra combinators ****************************************)

(** like >>>, but returns the first arg and ignores the result
    of the second **)
let ( <<< ) p1 p2 =
  p1 >>= fun x -> p2 >>> P.return x

(** post_map in reverse order **)
let ( => ) x f = P.post_map f x

(** collect two things and put them in a pair **)
let ( <&> ) p1 p2 =
  p1 >>= fun x -> p2 => fun y -> x, y


(* character patterns ***************************************)

let nospcrlfcl = C.none_of ['\x00'; '\r'; '\n'; ' '; ':']
let nospcrlfat = C.none_of ['\x00'; '\r'; '\n'; ' '; '@']
let nospcrlf   = C.none_of ['\x00'; '\r'; '\n'; ' ']
let nocrlf     = C.none_of ['\x00'; '\r'; '\n']
let alphanum   = C.letter <|> C.digit
let special    = P.one_of ['['; ']'; '\\'; '`'; '_'; '^'; '{'; '|'; '}']
let nickchr    = alphanum <|> special <|> C.char '-'
let hostchr    = alphanum <|> C.char '.'


(* parsers **************************************************)

let hostname = P.filter (List.mem '.') (~+ hostchr) => String.of_list
let username = (~+ nospcrlfat) => String.of_list
let nickname = P.cons
                 (C.letter <|> special)
                 (~+ nickchr)
               => String.of_list

(** prefix ":xyz... " **)
let prefix =
  let pfx_server =
    hostname
    => fun s ->
       Msg.Prefix_server s
  in

  let maybe_un = P.maybe (C.char '!' >>> username) in
  let maybe_hn = P.maybe (C.char '@' >>> hostname) in
  let pfx_user =
    (nickname <&> maybe_un <&> maybe_hn)
    => fun ((nick,user),host) ->
       Msg.Prefix_user (nick, user, host)
  in

  C.char ':'
  >>> (pfx_server <|> pfx_user)
  <<< C.char ' '

(** command: "abc..."/"123" **)
let command =
  (~+ C.letter) <|> (C.digit ^^ 3)
  => String.of_list

(** params: " xy zw :trailing..." **)
let params =
  let middle_param =
    C.char ' '
    >>> P.cons
          nospcrlfcl
          (P.must (~* nospcrlf))
    => String.of_list
  in
  let trailing_param =
    C.string " :"
    >>> (~* nocrlf)
    => String.of_list
  in
  (~* middle_param) <&> (~? trailing_param)
  => function
    | ps, Some p' -> ps @ [p']
    | ps, None    -> ps

(** entire message **)
let raw_message =
  (~? prefix <&> command <&> params) <<< C.string "\r\n"
  => fun ((pfx,cmd),ps) ->
     { Msg.raw_pfx = pfx;
       Msg.raw_cmd = cmd;
       Msg.raw_params = ps }




(* tests ****************************************************)

module Test = struct
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

  let tests =
    "test.msg_parse"
    >::: [ "prefix" >:: prefix_test
         ; "params" >:: params_test
         ; "message" >:: message_test
         ]
end
