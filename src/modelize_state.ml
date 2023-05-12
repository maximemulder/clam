module NameKey = struct
  type t = string
  let compare = String.compare
end

module NameMap = Map.Make(NameKey)

module NameSet = Set.Make(Scope.NameKey)

let extract key map =
  let value = NameMap.find key map in
  let map = NameMap.remove key map in
  (value, map)
