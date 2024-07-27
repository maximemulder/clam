open Base
open Identity

module type READER = sig
  type reader
end

module ReaderT (M: MONAD) (R: READER) : sig
  include TRANS
    with type 'a t = R.reader -> 'a M.t
     and type 'a m := 'a M.t

  val get : R.reader t
end

module Reader (R: READER) : module type of ReaderT(Identity)(R)
