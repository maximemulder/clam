module type MONAD = sig
  type 'a t
  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
end

module Monad (M: MONAD) = struct
  include M

  let (let*) = bind

  let rec map f xs =
    match xs with
    | [] -> return []
    | x :: xs ->
      let* x = f x in
      let* xs = map f xs in
      return (x :: xs)
end

module type STATE = sig
  type s
end

module StateMonad (S: STATE) = struct
  open S

  type 'a t = s -> ('a * s)

  let return a s = (a, s)

  let bind m f =
    fun s ->
      let (a, state_1) = m s in
      let (b, state_2) = f a state_1 in
      (b, state_2)
end
