module NameKey = struct
  type t = string
  let compare = String.compare
end

module NameMap = Map.Make(NameKey)

module NameSet = Set.Make(NameKey)
