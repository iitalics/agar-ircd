open Batteries

(**
    Common types, type aliases, and functions for
    simple IRC primitives like messages and message prefixe
 **)

type nick_name = string
type user_name = string
type host_name = string

type prefix = nick_name option * user_name option * host_name option

(** messages prefixes **)
module Prefix = struct
  type t = prefix

  (** an empty prefix. **)
  let empty : t = None, None, None
  (** is the given prefix empty? **)
  let is_empty = ( = ) empty

  (** [of_host h] creates a prefix with just hostname [h]. **)
  let of_host hos = None, None, Some hos
  (** [of_user_host u h] creates a prefix with username [u] and hostname [h]. **)
  let of_user_host usr hos = None, Some usr, Some hos
  (** [of_triple n u h] creates a prefix with nickname [n],
      username [u] and hostname [h]. **)
  let of_triple nic usr hos = Some nic, Some usr, Some hos

  (** prints a prefix as [<nick>![<user>@[<host>]]] **)
  let print out (pre_nic, pre_usr, pre_hos) =
    let open Printf in
    pre_nic |> Option.may (fprintf out "%s!");
    pre_usr |> Option.may (fprintf out "%s@");
    pre_hos |> Option.may (fprintf out "%s")

  (** [to_string pfx] converts prefix [pfx] to a string using [print] **)
  let to_string pfx =
    let out = IO.output_string () in
    print out pfx;
    IO.close_out out

end


type msg =
  { prefix : prefix;
    command : string;
    params : string list }

module Msg = struct
  type t = msg

  (** [simple c pms] creates a message with command [c] and
      parameters [pms]. **)
  let simple c pms =
    { prefix = Prefix.empty;
      command = c;
      params = pms }

  (** [simple1 c pm] creates a message with command [c] and a single
      parameter [pm]. **)
  let simple1 c pm =
    { prefix = Prefix.empty;
      command = c;
      params = [pm] }

  (** [format c pre fmt ...] creates a message with command [c],
      parameters [pre] follow by a final parameter using the format
      string [fmt] and the following arguments. **)
  let format c pre fmt =
    Printf.ksprintf (fun s ->
        simple c (pre @ [s]))
      fmt

  (* TODO: if it becomes a bottleneck, intern reply command names. *)

  (** [reply n pms nick] creates a "reply" message, using the
      number [n] as the reply command, with [nick] as the first
      parameter (the target of the reply), followed by parameters
      [pms]. the argument order is flipped around because this
      allows higher-order manipulation of replies without immediately
      passing the nick. **)
  let reply n pms nick =
    simple (string_of_int n) (nick :: pms)

  (** combination of [reply] and [format]. **)
  let replyf n pre fmt =
    Printf.ksprintf (fun s ->
        reply n (pre @ [s]))
      fmt

  (** prints in a format that can be parsed back. **)
  let print out m =
    let open Printf in
    let rec write_args = function
      | [] -> ()
      | [pm] ->
         Printf.fprintf out " :%s" pm
      | pm::rest ->
         Printf.fprintf out " %s" pm;
         write_args rest
    in
    if not (Prefix.is_empty m.prefix) then
      begin
        IO.write out ':';
        Prefix.print out m.prefix;
        IO.write out ' ';
      end;
    fprintf out "%s" m.command;
    write_args m.params

  (** [to_string m] converts message [m] to a string using [print]. **)
  let to_string m =
    let out = IO.output_string () in
    print out m;
    IO.close_out out

end
