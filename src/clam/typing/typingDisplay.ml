open Utils
open Model

let rec display type' =
  match snd type' with
  | TypeAny     -> "Any"
  | TypeVoid    -> "Void"
  | TypeBool    -> "Bool"
  | TypeInt     -> "Int"
  | TypeChar    -> "Char"
  | TypeString  -> "String"
  | TypeVar var -> var.type_var_param.param_type_name
  | TypeAbsExpr abs ->
    let params = List.map display abs.type_abs_expr_params in
    "(" ^ (String.concat ", " params) ^ ") -> " ^ (display abs.type_abs_expr_ret)
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
  | TypeAbs abs ->
    let params = List.map (fun param -> param.param_type_name ^ ": " ^ display param.param_type) abs.type_abs_params in
    "[" ^ (String.concat ", " params) ^ "] " ^ (display abs.type_abs_body)
  | TypeApp app ->
    let args = List.map display app.type_app_args in
    (display app.type_app_type) ^ " [" ^ (String.concat ", " args) ^ "]"
