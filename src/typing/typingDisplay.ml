open Collection
open Model

let rec display type' =
  match type' with
  | TypeVar { param_type_name; _ } -> param_type_name
  | TypeAny     -> "Any"
  | TypeVoid    -> "Void"
  | TypeBool    -> "Bool"
  | TypeInt     -> "Int"
  | TypeChar    -> "Char"
  | TypeString  -> "String"
  | TypeAbsExpr (params, expr) ->
    let params = List.map display params in
    "(" ^ (String.concat ", " params) ^ ") -> " ^ (display expr)
  | TypeAbsExprType (params, expr) ->
    let params = List.map (fun param -> param.param_type_name ^ ": " ^ display param.param_type) params in
    "[" ^ (String.concat ", " params) ^ "] -> " ^ (display expr)
  | TypeTuple types ->
    let types = List.map display types in
    "(" ^ (String.concat ", " types) ^ ")"
  | TypeRecord attrs ->
    let attrs = List.map (fun (name, attr) -> name ^ ": " ^ display attr.attr_type) (List.of_seq (NameMap.to_seq attrs)) in
    "{" ^ (String.concat ", " attrs) ^ "}"
  | TypeInter (left, right) ->
    (display left) ^ " & " ^ (display right)
  | TypeUnion (left, right) ->
    (display left) ^ " | " ^ (display right)
  | TypeAbs (params, type') ->
    let params = List.map (fun param -> param.param_type_name ^ ": " ^ display param.param_type) params in
    "[" ^ (String.concat ", " params) ^ "] " ^ (display type')
  | TypeApp (type', args) ->
    let args = List.map display args in
    (display type') ^ " [" ^ (String.concat ", " args) ^ "]"
