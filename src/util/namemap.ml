module StringKey = struct
  type t = string
  let compare = String.compare
end

module IntKey = struct
  type t = int

  let compare = Int.compare
end

module NameMap = Map.Make(StringKey)

module NameSet = Set.Make(StringKey)

module IntMap = Map.Make(IntKey)
