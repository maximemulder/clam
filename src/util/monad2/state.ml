open Base
open Func
open Identity

module type STATE = sig
  type state
end

module StateT (M: MONAD) (S: STATE) = struct
  include Make(struct
    type 'a t = S.state -> ('a * S.state) M.t

    let return a s =
      M.return (a, s)

    let bind m f s =
      M.bind (m s) (uncurry f)
  end)

  let get s =
    M.return (s, s)

  let put s _ =
    M.return ((), s)

  let modify f s =
    M.return ((), f s)

  let lift m s =
    M.bind m (fun a -> return a s)
end

module State = StateT(Identity)
