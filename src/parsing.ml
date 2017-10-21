open Batteries
module P = ParserCo
module C = CharParser
open ParserCo.Infix

type 'a p = (char, 'a, C.position) P.t

let ( <<< ) p1 p2 =
  p1 >>= (fun x -> p2 >>> P.return x)



let nospcrlfcl = C.none_of ['\x00'; '\r'; '\n'; ' '; ':']

let prefix : string option p =
  P.maybe (C.char ':'
           >>> ~+ nospcrlfcl
           <<< C.char ' '
           |> P.post_map String.of_list)

let command =
  (~+ C.letter) <|> (C.digit ^^ 3)
