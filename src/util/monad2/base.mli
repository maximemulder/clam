module type BASE = sig
  type 'a t

  val return: 'a -> 'a t

  val bind: 'a t -> ('a -> 'b t) -> 'b t
end

module type MONAD = sig
  include BASE

  val (let*) : 'a t -> ('a -> 'b t) -> 'b t
end

module type TRANS = sig
  type 'a m
  type 'a t

  val lift : 'a m -> 'a t
end

module Make (B: BASE) : MONAD
  with type 'a t = 'a B.t
