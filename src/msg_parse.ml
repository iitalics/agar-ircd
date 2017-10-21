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
  let maybe_un = P.maybe (C.char '!' >>> P.must username) in
  let maybe_hn = P.maybe (C.char '@' >>> P.must hostname) in

  let pfx_user =
    (nickname <&> maybe_un <&> maybe_hn)
    => fun ((nick,user),host) ->
       Msg.Prefix_user (nick, user, host)
  in

  let pfx_server =
    hostname
    => fun s ->
       Msg.Prefix_server s
  in

  let pfx = pfx_server <|> pfx_user in
  C.char ':' >>> (P.must pfx <<< C.char ' ')

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
    >>> P.must (~* nocrlf)
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
