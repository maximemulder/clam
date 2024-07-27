open Base
open Identity

module type RESULT = sig
  type error
end

module ResultT (M: MONAD) (R: RESULT) = struct
  include Make(struct
    type 'a t = ('a, R.error) result M.t

    let return a =
      M.return (Ok a)

    let bind m f =
      M.bind m (fun m ->
        match m with
        | Ok v ->
          f v
        | Error e ->
          M.return (Error e)
    )
  end)

  let lift m =
    M.bind m return

  let fail e =
    M.return (Error e)
end

module Result = ResultT(Identity)
