open Batteries

(** monad interface **)
module type SIG = sig

  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t

end

(** infix operators for monads **)
module Infix(M : SIG) = struct

  let ( >>= ) = M.bind
  let ( >> ) f g = f >>= fun _ -> g
  let ( => ) x f = M.map f x

end

(** utility functions for monads **)
module Extras(M : SIG) = struct
  module I = Infix(M)
  open I

  let rec m_for l f = match l with
    | [] -> M.return ()
    | x::xs -> f x >> m_for xs f

end


module Identity = struct

  type 'a t = 'a
  let return x = x
  let bind x f = f x
  let map f x = f x

end
