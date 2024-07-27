open Base
open Identity

module type RESULT = sig
  type error
end

module ResultT (M: MONAD) (R: RESULT) : sig
  include MONAD
    with type 'a t = ('a, R.error) result M.t

  include TRANS
    with type 'a m := 'a M.t
     and type 'a t := 'a t

  val fail : R.error -> 'a t
end

module Result (R: RESULT) : module type of ResultT(Identity)(R)
