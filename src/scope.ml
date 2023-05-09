open Expr
open Type

module NameKey = struct
  type t = string
  let compare = String.compare
end

module NameMap = Map.Make(NameKey)

type scope = {
  parent: scope option;
  types: type' NameMap.t;
  exprs: expr NameMap.t;
}

let empty =
  {
    parent = Option.None;
    types = NameMap.empty;
    exprs = NameMap.empty;
  }

let empty_child parent =
  {
    parent = Some parent;
    types = NameMap.empty;
    exprs = NameMap.empty;
  }

let rec find_type name scope =
  let type' = NameMap.find_opt name scope.types in
  match (type', scope.parent) with
  | (Some type', _)     -> Some type'
  | (None, Some parent) -> find_type name parent
  | (None, None)        -> None

let add_type name type' scope =
  let types = NameMap.add name type' scope.types in
    { scope with types }
