open Batteries

(**
    Signature for monads.
 *)
module type SIG = sig
  type 'a t
  val pure : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(**
    Extra monad utilities, such as infix operators.
*)
module Extra(M : SIG) = struct

  let ( >>= ) = M.bind
  let ( >> ) a b = a >>= fun _ -> b

  (** iterate through a list, carrying monadic
      effects for each element. *)
  let rec m_iter f = function
    | [] -> M.pure ()
    | x::xs -> f x >>= fun _ -> m_iter f xs

end

(**
    Functions for monad-like results within another monad.
*)
module Result(M : SIG) = struct

  let ( >>=? ) r f =
    match r with
    | Ok x -> f x
    | Bad b -> Bad b

  let ( >>? ) r f =
    match r with
    | Ok _ -> f
    | Bad b -> Bad b

end
