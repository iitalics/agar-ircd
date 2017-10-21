open Batteries
module P = ParserCo
module C = CharParser
open ParserCo.Infix

(* parsers from chars to stuff *)
type 'a p = (char, 'a, C.position) P.t

(* some extra combinators *)

(** like >>>, but returns the first arg and ignores the result of the second **)
let ( <<< ) p1 p2 =
  p1 >>= (fun x -> p2 >>> P.return x)

(** collect two things and put them in a pair **)
let ( <&> ) p1 p2 =
  p1 >>= (fun x -> P.post_map (fun y -> x, y) p2)

(** character patterns **)
let nospcrlfcl = C.none_of ['\x00'; '\r'; '\n'; ' '; ':']
let nospcrlf   = C.none_of ['\x00'; '\r'; '\n'; ' ']
let nocrlf     = C.none_of ['\x00'; '\r'; '\n']

(** prefix ":xyz... " **)
let prefix : string option p =
  ~? (C.char ':'
      >>> P.must (~+ nospcrlf
                  <<< C.char ' '
                  |> P.post_map String.of_list))

(** command: "abc..."/"123" **)
let command =
  (~+ C.letter) <|> (C.digit ^^ 3)
  |> P.post_map String.of_list

(** params: " xy zw :trailing..." **)
let params =
  let middle_param   =
    C.char ' '
    >>> P.cons
          nospcrlfcl
          (~* nospcrlf)
    |> P.post_map String.of_list
  in
  let trailing_param =
    C.string " :" >>> (~* nocrlf)
    |> P.post_map String.of_list
  in
  let combine (ps,maybe_last) =
    match maybe_last with
    | Some p -> ps @ [p]
    | None -> ps
  in
  (~* middle_param) <&> (~? trailing_param)
  |> P.post_map combine

(** entire message **)
let raw_message =
  (prefix <&> command <&> params) <<< C.string "\r\n"
  >>= (fun ((pfx,cmd),ps) ->
    P.return { Msg.raw_pfx = pfx;
               Msg.raw_cmd = cmd;
               Msg.raw_params = ps })



(****************************************************)
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
    | Bad x -> assert_failure (fmt "failed to parse %S" s)

  (** assert that parsing 's' with 'p' fails **)
  let parse_fails p s =
    match C.parse p s with
    | Ok a -> assert_failure (fmt "expected parse %S to fail" s)
    | Bad x -> assert_bool "ok" true


  let pfx_test _ = begin
      let pp = function
        | None -> "None"
        | Some x -> fmt "Some(%S)" x
      in
      parses_to pp prefix    ":a " (Some "a");
      parses_to pp prefix    ":0:1:2:3 " (Some "0:1:2:3");
      parse_fails prefix     ":";
      parse_fails prefix     ":abcfds\r\n";
      parses_to pp prefix    "PRIVMSG a" None;
    end

  let params_test _ = begin
      let pp ss =
        String.concat ";" (List.map (fmt "%S") ss)
      in
      parses_to pp params    " a b c" ["a";"b";"c"];
      parses_to pp params    "" [];
      parses_to pp params    " a :b c" ["a";"b c"];
    end

  let message_test _ = begin
      let msg ?pfx cmd pars =
        { Msg.raw_pfx = pfx;
          Msg.raw_cmd = cmd;
          Msg.raw_params = pars }
      in
      let pp m =
        let open Msg in
        fmt "%s%S %s"
          (match m.raw_pfx with
           | Some x -> fmt "{prefix %S}" x
           | None -> "")
          m.raw_cmd
          (String.concat ";" (List.map (fmt "%S") m.raw_params))
      in

      parses_to pp raw_message     "QUIT\r\n" (msg "QUIT" []);
      parses_to pp raw_message     "PRIVMSG a b\r\n" (msg "PRIVMSG" ["a";"b"]);
      parses_to pp raw_message     "PRIVMSG a :b c\r\n" (msg "PRIVMSG" ["a";"b c"]);
      parses_to pp raw_message     "PRIVMSG a b :c d e\r\n" (msg "PRIVMSG" ["a";"b";"c d e"]);
      parses_to pp raw_message     ":origin 123\r\n" (msg ?pfx:(Some "origin") "123" []);
      parses_to pp raw_message     ":place QUIT :eating lunch\r\n"
        (msg ?pfx:(Some "place")
           "QUIT" ["eating lunch"]);

      parse_fails raw_message      "QUIT";
      parse_fails raw_message      "1234\r\n";
      parse_fails raw_message      "PRIVMSG a \rb\r\n";
      parse_fails raw_message      "PRIVMSG a\x00bc\r\n";
    end

  let tests =
    "test.msg_parse"
    >::: [ "prefix" >:: pfx_test
         ; "params" >:: params_test
         ; "message" >:: message_test
         ]
end
