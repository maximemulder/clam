module type FUNCTOR = sig
  type 'a t
  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
end

module Functor (F: FUNCTOR) = struct
  include F

  let (let+) = bind

  let rec map_list f xs =
    match xs with
    | [] -> return []
    | x :: xs ->
      let+ x = f x in
      let+ xs = map_list f xs in
      return (x :: xs)

  let map_option f x =
    match x with
    | None -> return None
    | Some x ->
      let+ x = f x in
      return (Some x)
end

module type CONTEXT = sig
  type c
end

module ContextFunctor (C: CONTEXT) = struct
  open C

  type 'a t = c -> 'a

  let return a _ = a

  let bind m f =
    fun c ->
      let a = m c in
      let b = f a c in
      b
end
