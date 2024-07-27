open Base
open Identity

module type RESULT = sig
  type error
end

module ResultT (M: MONAD) (R: RESULT) : sig
  include TRANS
    with type 'a t = ('a, R.error) result M.t
     and type 'a m := 'a M.t

  val fail : R.error -> 'a t
end

module Result : module type of ResultT(Identity)
