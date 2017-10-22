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

let sp =
  P.ignore_one_plus (C.char ' ')

let nickname =
  P.cons
    (C.letter <|> special)
    (~* nickchr)
  => String.of_list

let username =
  (~+ nospcrlfat)
  => String.of_list

let hostname =
  (~+ hostchr)
  |> P.filter (fun chrs ->
         List.length chrs > 2
         && List.mem '.' chrs)
  => String.of_list


(** prefix
      :<hostname>
    or
      :<nick>[!<user>][@<host>]
 **)
let prefix =

  let maybe_un = ~? (C.char '!' >>> P.must username) in
  let maybe_hn = ~? (C.char '@' >>> P.must hostname) in

  let nick_user_host =
    (nickname <&> maybe_un <&> maybe_hn)
    => fun ((n,u),h) ->
       Msg.Prefix_user (n, u, h)
  in

  let servername =
    hostname
    => fun s ->
       Msg.Prefix_server s
  in

  C.char ':' >>= fun _ ->
  P.must
    (~+ nospcrlf >>= fun chrs ->
     match P.run (servername <|> nick_user_host)
             (C.source_of_enum (List.enum chrs)) with
     | Ok p -> P.return p
     | Bad _ -> P.fail)


(** command
      <letter>+
    or
      <digit x 3>
 **)
let command =
  ~? sp >>> ((~+ C.uppercase) <|> (C.digit ^^ 3))
  => String.of_list


(** params
     <middle>+ [:<any>*]
 **)
let params =

  let middle =
    sp >>> (nospcrlfcl >:: ~* nospcrlf)
    => String.of_list
  in

  let trailing =
    sp
    >>> C.char ':'
    >>> ~* nocrlf
    => String.of_list
  in

  (~* middle <&> ~? trailing)
  => function
    | ps, Some p -> ps @ [p]
    | ps, None   -> ps


(** message:
      <prefix>? <command> <params> \r\n
 **)
let message =
  (~? prefix <&> command <&> params) <<< C.string "\r\n"
  => fun ((pfx,cmd),pars) ->
     { Msg.raw_pfx = pfx;
       Msg.raw_cmd = cmd;
       Msg.raw_params = pars }
