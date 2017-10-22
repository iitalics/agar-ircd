open BatteriesThread
open Batteries

module Console = struct
  exception Quit

  type 'a t = unit -> 'a
  let run f = f ()

  let bind m f = f (m ())
  let return = const
  let map = (%)

  let con_id () = 0
  let quit () = raise Quit
  let send tgt s () =
    Printf.printf "sending to %s:\n" (Routing.string_of_target tgt);
    print_string s;
    flush IO.stdout

end



let _ =
  let module C = Console in
  let module H = Child.Make(C) in

  C.run H.init;
  try
    while true do
      print_string "> ";
      flush IO.stdout;

      let line = read_line () ^ "\r\n" in
      C.run (H.recv line)
    done
  with
  | C.Quit -> print_string "quit.\n"
  | End_of_file -> ()
