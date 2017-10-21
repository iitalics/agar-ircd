open BatteriesThread
open Batteries

let main () =
  Printf.printf "Hello, world!\n"

let _ =
  let open Irc_common in
  let module M = Msg in
  let module MP = Msg_parser in

  main ()
