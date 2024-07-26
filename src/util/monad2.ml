module type MONAD = sig
  type 'a t
  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
end

module Syntax (M: MONAD) = struct
  let (let*) = M.bind
end

module Util (M: MONAD) = struct
  open M
  open Syntax(M)

  (* Monad utilitary functions here *)
end

module Monad (M: MONAD) = struct
  include M
  include Syntax(M)
  module Monad = Util(M)
end

module Identity = Monad(struct
  type 'a t = 'a

  let return a =
    a

  let bind m f =
    f m

end)

module type RESULT = sig
  type error
end

module Result (R: RESULT) = Monad(struct
  type 'a t = ('a, R.error) result

  let return a = Ok a

  let bind m f =
    match m with
    | Ok  v -> f v
    | Error e -> Error e

end)

module type READER = sig
  type reader
end

module Reader (R: READER) = Monad(struct
  type 'a t = R.reader -> 'a

  let return a _ =
    a

  let bind m f r =
    f (m r) r

  let get s =
    s

end)

module type STATE = sig
  type state
end

module State (S: STATE) = Monad(struct
  type 'a t = S.state -> ('a * S.state)

  let return a s = (a, s)

  let bind m f s =
    let (a, s1) = m s in
    f a s1

  let get s =
    s, s

  let put s _ =
    (), s

  let modify f s =
    (), f s

end)
