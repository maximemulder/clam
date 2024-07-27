open Base
open Identity

module type READER = sig
  type reader
end

module ReaderT (M: MONAD) (R: READER) : sig
  include MONAD
    with type 'a t = R.reader -> 'a M.t

  include TRANS
    with type 'a m := 'a M.t
     and type 'a t := 'a t

  val get : R.reader t
end

module Reader (R: READER) : module type of ReaderT(Identity)(R)
