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

  let option_meet f x y =
    match x, y with
    | Some x, Some y ->
      let* z = f x y in
      return (Some z)
    | _ ->
      return None

  let option_join f x y =
    match x, y with
    | Some x, Some y ->
      let* z = f x y in
      return (Some z)
    | Some x, None ->
      return (Some x)
    | None, Some y ->
      return (Some y)
    | _ ->
      return None

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

  let rec list_map2 f xs ys =
    match xs, ys with
    | [], [] -> return []
    | x :: xs, y :: ys ->
      let* z = f x y in
      let* zs = list_map2 f xs ys in
      return (z :: zs)
    | _ -> invalid_arg "Monad.list_map2"

  let rec iter_list2 f xs ys =
    match (xs, ys) with
    | ([], []) -> return ()
    | (x :: xs, y :: ys) ->
      let* () = f x y in
      iter_list2 f xs ys
    | _ -> invalid_arg "Monad.iter_list2"

  let map_iter f xs =
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

  let rec list_fold f a xs =
    match xs with
    | [] ->
      return a
    | x :: xs ->
      let* r = list_fold f a xs in
      f r x

  let rec list_product f acc l1 l2 =
    match l1, l2 with
    | [], _ | _, [] ->
      return acc
    | h1 :: t1, h2 :: t2 ->
      let* h = f h1 h2 in
      let* acc = list_product f (h :: acc) t1 l2 in
      list_product f acc [h1] t2

  let rec list_option_meet xs f =
    match xs with
    | [x] ->
      return x
    | x :: xs ->
      let* y = list_option_meet xs f in
      option_meet f x y
    | _ ->
      invalid_arg "list_option_meet"

  let rec list_option_join xs f =
    match xs with
    | [x] ->
      return x
    | x :: xs ->
      let* y = list_option_join xs f in
      option_join f x y
    | _ ->
      invalid_arg "list_option_join"

  let list_product f l1 l2 =
    let* l = list_product f [] l1 l2 in
    return (List.rev l)

  let rec list_reduce f xs =
    match xs with
    | [x] ->
      return x
    | x :: xs ->
      let* y = list_reduce f xs in
      f x y
    | _ -> invalid_arg "Util.list_reduce"

  let rec list_collapse n xs ys zs f =
    match xs with
    | [] -> (
      match ys with
      | [] ->
        return (zs @ [n])
      | y :: ys ->
        list_collapse y ys [] (zs @ [n]) f)
    | x :: xs ->
      let* o = f n x in
      match o with
      | Some n -> list_collapse n (xs @ ys @ zs) [] [] f
      | None -> list_collapse n xs (ys @ [x]) zs f

  let list_collapse f xs =
    match xs with
    | x :: xs -> list_collapse x xs [] [] f
    | _ -> invalid_arg "list_collapse"

  let map_any f xs =
    let f = (fun (_, v) -> f v) in
    let xs = List.of_seq (NameMap.to_seq xs) in
    list_any f xs

  let map_all f xs =
    let f = (fun (_, v) -> f v) in
    let xs = List.of_seq (NameMap.to_seq xs) in
    list_all f xs

  let map_fold f a xs =
    let f = (fun a (_, v) -> f a v) in
    let xs = List.of_seq (NameMap.to_seq xs) in
    list_fold f a xs

  let map_join f xs ys =
    let zs = NameMap.merge (fun _ x y -> Some (x, y)) xs ys in
    let zs = List.of_seq (NameMap.to_seq zs) in
    let* zs = list_map (fun (k, (v1, v2)) ->
      let* z = option_join f v1 v2 in
      return (k, Option.get z)
    ) zs in
    return (NameMap.of_seq (List.to_seq zs))
end

module type STATE = sig
  type s
end

module StateMonad (S: STATE) = struct
  open S

  include Monad(struct
    type 'a t = s -> ('a * s)

    let return a s = (a, s)

    let bind m f s =
      let (a, s1) = m s in
      f a s1
  end)

  let get s = s, s

  let put s _ = (), s
end

module type READER = sig
  type r
end

module ReaderMonad (R: READER) = struct
  open R

  include Monad(struct
    type 'a t = r -> 'a

    let return a _ = a

    let bind r f c =
      f (r c) c
  end)

  let get s = s
end
