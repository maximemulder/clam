open Base
open Identity

module type STATE = sig
  type state
end

module StateT (M: MONAD) (S: STATE) : sig
  include TRANS
    with type 'a t = S.state -> ('a * S.state) M.t
     and type 'a m := 'a M.t

  val get : S.state t

  val put : S.state -> unit t

  val modify : (S.state -> S.state) -> unit t
end

module State (S: STATE) : module type of StateT(Identity)(S)
