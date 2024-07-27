open Base
open Identity

module type READER = sig
  type reader
end

module ReaderT (M: MONAD) (R: READER) = struct
  include Make(struct
    type 'a t = R.reader -> 'a M.t

    let return a _ =
      M.return a

    let bind m f r =
      M.bind (m r) (fun a -> f a r)
  end)

  let lift m r =
    M.bind m (fun a -> return a r)

  let get r =
    M.return r
end

module Reader = ReaderT(Identity)
