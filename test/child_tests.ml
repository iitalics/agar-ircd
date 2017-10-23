open Batteries
open OUnit2

(** monad for mock testing **)
module Queue_mock = struct
  exception Aborted of [ `quit
                       | `sent of int * string
                       ]

  type mock = {
      con_id : int;
      mutable state : Child.state;
      mutable queue :
                [ `send of string
                | `recv of int * string
                | `quit ] list;
    }

  type 'a t = mock -> 'a
  let bind f g m = g (f m) m
  let return = const
  let map = (%)

  let con_id m = m.con_id
  let get_s m = m.state
  let put_s s m = m.state <- s

  let quit _ = raise (Aborted `quit)

  let send con str m =
    match m.queue with
    | (`recv (i, str'))::q' when con = i
                                 && str = str' ->
       m.queue <- q'

    | _ -> raise @@ Aborted (`sent (con, str))

end

(** functon for running mocker with the givens state *)
let mock m =
  let module M = Queue_mock in
  let module H = Child.Make(M) in
  let open M in
  let pp = function
    | `send s -> Printf.sprintf "[client <- %S]" s
    | `recv (c,s) -> Printf.sprintf "[%S -> #%d]" s c
    | `quit -> "[quit]"
  in
  try
    while true do
      match m.queue with
      | (`send str)::q' ->
         m.queue <- q';
         H.recv str m

      | _ -> raise (Aborted `quit)
    done
  with
  | Aborted `quit ->
     assert_equal ~printer:pp
       (List.at_opt m.queue 0 |> Option.default `quit)
       `quit

  | Aborted (`sent (c, s)) ->
     assert_equal ~printer:pp
       (List.at_opt m.queue 0 |> Option.default `quit)
       (`recv (c, s))


(** run mock with presupplied queue **)
let queue_mock ~queue:q =
  let module M = Queue_mock in
  let module H = Child.Make(M) in
  let open M in
  mock { con_id = 0;
         state = H.init ();
         queue = q }


let quit_test _ = begin
    queue_mock [`send "QUIT\r\n";
                `recv (0, ":test.irc ERROR :Bye\r\n");
                `quit ]
  end


let err_test_1 _ = begin
    queue_mock [`send "FOO\r\n";
                `recv (0, ":test.irc 421 * FOO :Unknown command\r\n") ];

    queue_mock [`send "NICK milo\r\n";
                `send "FOO\r\n";
                `recv (0, ":test.irc 421 milo FOO :Unknown command\r\n") ];

    queue_mock [`send "NICK 123\r\n";
                `recv (0, ":test.irc 432 * 123 :Erroneous nickname\r\n") ];
  end



let tests =
  "test.child"
  >::: [
      "quit" >:: quit_test;
      "err_1" >:: err_test_1;
    ]
