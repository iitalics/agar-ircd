open Batteries

type target
  = To_con of int
  | To_nick of string
  | To_chan of string

let string_of_target =
  let open Printf in
  function
  | To_con i -> sprintf "[%d]" i
  | To_nick n -> sprintf "user:%s" n
  | To_chan c -> sprintf "chan:%s" c
