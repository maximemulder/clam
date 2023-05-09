open Model

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

let rec find_expr name scope =
  let expr = NameMap.find_opt name scope.exprs in
  match (expr, scope.parent) with
  | (Some expr, _)     -> Some expr
  | (None, Some parent) -> find_expr name parent
  | (None, None)        -> None

let add_type name type' scope =
  let types = NameMap.add name type' scope.types in
    { scope with types }

let add_expr name expr scope =
  let exprs = NameMap.add name expr scope.exprs in
    { scope with exprs }
