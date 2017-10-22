open BatteriesThread
open Batteries

module Console = struct
  exception Quit

  type 'a t = unit -> 'a
  let state = ref None

  let bind m f = f (m ())
  let return = const
  let map = (%)

  let con_id () = 0
  let get_s () = Ref.oget_exn state
  let put_s s () = state := Some s

  let quit () = raise Quit
  let send i s () =
    Printf.printf "sending to con #%d:\n" i;
    print_string s;
    flush IO.stdout

  let run f = f ()
end



let _ =
  let module C = Console in
  let module H = Child.Make(C) in

  C.state := Some (H.init ());
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
