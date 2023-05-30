module NameKey = struct
  type t = string
  let compare = String.compare
end

module NameMap = Map.Make(NameKey)

module NameSet = Set.Make(NameKey)

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

let map_option2 x y f =
  match (x, y) with
  | (Some x, Some y) -> Some (f x y)
  | _ -> None

let join_option2 x y f =
  match (x, y) with
  | (Some x, Some y) -> Some (f x y)
  | (Some x, None) -> Some x
  | (None, Some y) -> Some y
  | _ -> None
