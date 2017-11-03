open Batteries

(**
    Common types, type aliases, and functions for
    simple IRC primitives like messages and message prefixes.
 *)

type nick_name = string
type user_name = string
type real_name = string
type host_name = string

type prefix = nick_name option * user_name option * host_name option

let server_domain = ref "agar.irc"

(** messages prefixes *)
module Prefix = struct
  type t = prefix

  (** an empty prefix. *)
  let empty : t = None, None, None
  (** is the given prefix empty? *)
  let is_empty = ( = ) empty

  (** [of_nick n] creates a prefix with just nickname [n]. *)
  let of_nick nic : t = Some nic, None, None
  (** [of_nick_host n h] creates a prefix with nickname [n] and hostname [h]. *)
  let of_nick_host nic hos : t = Some nic, None, Some hos
  (** [of_triple n u h] creates a prefix with nickname [n],
      username [u] and hostname [h]. *)
  let of_triple nic usr hos : t = Some nic, Some usr, Some hos

  (** prints a prefix as [<nick>![<user>@[<host>]]] *)
  let print out ((pre_nic, pre_usr, pre_hos) : t) =
    let open Printf in
    pre_nic |> Option.may (fprintf out "%s");
    pre_usr |> Option.may (fprintf out "!%s");
    pre_hos |> Option.may (fprintf out "@%s")

  (** [to_string pfx] converts prefix [pfx] to a string using [print] *)
  let to_string pfx =
    let out = IO.output_string () in
    print out pfx;
    IO.close_out out

  (** [of_string s] converts non-empty string [s] into a prefix. *)
  let of_string s : t =
    if String.contains s '@' then
      let lhs, hos = String.split s ~by:"@" in
      let nic, maybe_usr =
        if String.contains lhs '!' then
          let nic, usr = String.split lhs ~by:"!" in
          nic, Some usr
        else
          lhs, None
      in
      Some nic, maybe_usr, Some hos
    else
      Some s, None, None

end


type msg =
  { prefix : prefix;
    command : string;
    params : string list }

module Msg = struct
  type t = msg

  (** [simple c pms] creates a message with command [c] and
      parameters [pms]. *)
  let simple c pms =
    { prefix = Prefix.empty;
      command = c;
      params = pms }

  (** [simple1 c pm] creates a message with command [c] and a single
      parameter [pm]. *)
  let simple1 c pm =
    { prefix = Prefix.empty;
      command = c;
      params = [pm] }

  (** [format c pre fmt ...] creates a message with command [c],
      parameters [pre] follow by a final parameter using the format
      string [fmt] and the following arguments. *)
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
      passing the nick. *)
  let reply n pms nick =
    simple (string_of_int n) (nick :: pms)

  (** combination of [reply] and [format]. *)
  let replyf n pre fmt =
    Printf.ksprintf (fun s ->
        reply n (pre @ [s]))
      fmt

  (** [is_reply m] returns true if [m] is a reply message,
      e.g. has a 3-digit command name. *)
  let is_reply m =
    String.length m.command = 3
    && Char.is_digit m.command.[0]
    && Char.is_digit m.command.[1]
    && Char.is_digit m.command.[2]

  (** prints in a format that can be parsed back. *)
  let print out m =
    let open Printf in
    let rec write_args = function
      | [] -> ()
      | [pm] -> fprintf out " :%s" pm
      | pm::rest -> fprintf out " %s" pm; write_args rest
    in
    if not (Prefix.is_empty m.prefix) then
      begin
        IO.write out ':';
        Prefix.print out m.prefix;
        IO.write out ' ';
      end;
    fprintf out "%s" m.command;
    write_args m.params

  (** [to_string m] converts message [m] to a string using [print]. *)
  let to_string m =
    let out = IO.output_string () in
    print out m;
    IO.close_out out

  (** [of_string s] converts a string to a message. faises [Failure]
      if the string is formatted incorrectly. *)
  let of_string s =
    let module SS = Substring in
    try
      (* parse prefix *)
      let pfx, s' =
        if String.starts_with s ":" then
          let pfx_s, body_s = String.split (String.lchop ~n:1 s) ~by:" " in
          Prefix.of_string pfx_s, body_s
        else
          Prefix.empty, s
      in

      (* separate spaces between words *)
      let rec parse_args ?(first=false) s =
        match SS.getc s with
        | None -> []
        | Some (' ', s') -> parse_args s'
        | Some (':', s') when not first -> [SS.to_string s']
        | _ ->
           let s1, s2 = SS.splitl (fun c -> c <> ' ') s in
           (SS.to_string s1)::parse_args s2
      in
      let args = parse_args ~first:true (SS.of_string s') in

      (* validate command *)
      let valid_cmd =
        String.map
          (fun c ->
            if Char.is_digit c then
              c
            else if Char.is_letter c then
              Char.uppercase c
            else
              raise (Failure "valid_cmd"))
      in
      { prefix = pfx;
        command = valid_cmd (List.hd args);
        params = List.tl args }
    with
      _ -> raise (Failure "Irc.Msg.of_string")

end
