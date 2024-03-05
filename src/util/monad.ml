open Namemap

module type MONAD = sig
  type 'a t
  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
end

module Monad (M: MONAD) = struct
  include M

  let (let*) = bind

  let option_map f x =
    match x with
    | None -> return None
    | Some x ->
      let* x = f x in
      return (Some x)

  let rec list_iter f xs =
    match xs with
    | [] ->
      return ()
    | x :: xs ->
      let* () = f x in
      list_iter f xs

  let rec list_map f xs =
    match xs with
    | [] ->
      return []
    | x :: xs ->
      let* x = f x in
      let* xs = list_map f xs in
      return (x :: xs)

  let rec iter_list2 f xs ys =
    match (xs, ys) with
    | ([], []) -> return ()
    | (x :: xs, y :: ys) ->
      let* () = f x y in
      iter_list2 f xs ys
    | _ -> invalid_arg "Monad.iter_list2"

  let rec map_list2 f xs ys =
    match (xs, ys) with
    | ([], []) -> return []
    | (x :: xs, y :: ys) ->
      let* z = f x y in
      let* zs = map_list2 f xs ys in
      return (z :: zs)
    | _ -> invalid_arg "Monad.map_list2"

  let iter_map f xs =
    let f = (fun (_, v) -> f v) in
    let xs = List.of_seq (NameMap.to_seq xs) in
    list_iter f xs

  let map_map f xs =
    let f = (fun (k, v) -> let* v = f v in return (k, v)) in
    let xs = List.of_seq (NameMap.to_seq xs) in
    let* xs = list_map f xs in
    return (NameMap.of_seq (List.to_seq xs))

  let rec fold_list f a xs =
    match xs with
    | [] -> return a
    | x :: xs ->
      let* b = f a x in
      fold_list f b xs

  let rec compare_list2 f xs ys =
    match (xs, ys) with
    | ([], []) -> return true
    | (x :: xs, y :: ys) ->
      let* r = f x y in
      let* r2 = compare_list2 f xs ys in
      return (r && r2)
    | _ ->
      return false

  let rec list_all f xs =
    match xs with
    | [] ->
      return true
    | x :: xs ->
      let* r = f x in
      let* r2 = list_all f xs in
      return (r && r2)

  let rec list_any f xs =
    match xs with
    | [] ->
      return false
    | x :: xs ->
      let* r = f x in
      let* r2 = list_any f xs in
      return (r || r2)

  let map_all f xs =
    let f = (fun (_, v) -> f v) in
    let xs = List.of_seq (NameMap.to_seq xs) in
    list_all f xs
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
