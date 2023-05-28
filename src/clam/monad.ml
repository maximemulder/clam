open Utils

module type MONAD = sig
  type 'a t
  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
end

module Monad (M: MONAD) = struct
  include M

  let (let*) = bind

  let rec list_map f xs =
    match xs with
    | [] -> return []
    | x :: xs ->
      let* x = f x in
      let* xs = list_map f xs in
      return (x :: xs)

  let rec list_fold f a xs =
    match xs with
    | [] -> return a
    | x :: xs ->
      let* b = f a x in
      list_fold f b xs

  let map_map f xs =
    let f = (fun (k, v) -> let* v = f v in return (k, v)) in
    let xs = List.of_seq (NameMap.to_seq xs) in
    let* xs = list_map f xs in
    return (NameMap.of_seq (List.to_seq xs))

  let option_map f x =
    match x with
    | None -> return None
    | Some x ->
      let* x = f x in
      return (Some x)
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
      let (a, s1) = m s in
      f a s1
end

module type READER = sig
  type r
end

module ReaderMonad (R: READER) = struct
  open R

  type 'a t = r -> 'a

  let return a _ = a

  let bind r f =
    fun c -> f (r c) c
end

(* state let map:

  let (let+) m f =
  fun s ->
    let (a, s1) = m s in
    (f a, s1)
*)
