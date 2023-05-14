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
