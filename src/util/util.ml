include Namemap

type writer = string -> unit

module Monad = Monad

let flip f x y = f y x

let uncurry f (x, y) = f x y

let apply x f = f x

let extract key map =
  let value = NameMap.find key map in
  let map = NameMap.remove key map in
  (value, map)

let compare_lists compare list other =
  if List.compare_lengths list other != 0 then false else
  let pairs = List.combine list other in
  let compare_pair = fun (a, b) -> compare a b in
  List.for_all compare_pair pairs

let compare_maps compare map other =
  let list = List.of_seq (NameMap.to_seq map) in
  let other = List.of_seq (NameMap.to_seq other) in
  if List.compare_lengths list other != 0 then false else
  let pairs = List.combine list other in
  List.for_all (fun (entry, other_entry) ->
    (fst entry) = (fst other_entry) && compare (snd entry) (snd other_entry)
  ) pairs

let bool_then bool value =
  match bool with
  | true ->
    Some value
  | false ->
    None

let rec list_reduce f xs =
  match xs with
  | [x] -> x
  | x :: xs -> f x (list_reduce f xs)
  | _ -> invalid_arg "Util.list_reduce"

let option_join x y f =
  match x, y with
  | Some x, Some y -> Some (f x y)
  | Some x, None -> Some x
  | None, Some y -> Some y
  | _ -> None

let option_meet x y f =
  match x, y with
  | Some x, Some y -> Some (f x y)
  | _ -> None

let rec list_option_meet xs f =
  match xs with
  | [x] ->
    x
  | x :: xs ->
    option_meet x (list_option_meet xs f) f
  | _ ->
    invalid_arg "list_option_meet"

let rec list_option_join xs f =
  match xs with
  | [x] ->
    x
  | x :: xs ->
    option_join x (list_option_join xs f) f
  | _ ->
    invalid_arg "list_option_join"

let rec list_product acc f l1 l2 =
  match l1, l2 with
  | [], _ | _, [] ->
    acc
  | h1 :: t1, h2 :: t2 ->
    let acc = f h1 h2 :: acc in
    let acc = list_product acc f t1 l2 in
    list_product acc f [h1] t2

let list_product f l1 l2 =
  let l = list_product [] f l1 l2 in
  List.rev l

let rec list_collapse n xs ys zs f =
  match xs with
  | [] -> (
    match ys with
    | [] -> zs @ [n]
    | y :: ys -> list_collapse y ys [] (zs @ [n]) f)
  | x :: xs -> (
    match f n x with
    | Some n -> list_collapse n (xs @ ys @ zs) [] [] f
    | None -> list_collapse n xs (ys @ [x]) zs f)

let list_collapse f xs =
  match xs with
  | x :: xs -> list_collapse x xs [] [] f
  | _ -> invalid_arg "list_collapse"
