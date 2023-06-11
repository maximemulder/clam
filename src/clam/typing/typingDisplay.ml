open Utils
open Model

let rec display type' =
  match type' with
  | TypeAny _    -> "Any"
  | TypeVoid _   -> "Void"
  | TypeBool _   -> "Bool"
  | TypeInt _    -> "Int"
  | TypeChar _   -> "Char"
  | TypeString _ -> "String"
  | TypeVar var -> var.type_var_param.param_type_name
  | TypeAbsExpr abs ->
    let params = List.map display abs.type_abs_expr_params in
    "(" ^ (String.concat ", " params) ^ ") -> " ^ (display abs.type_abs_expr_body)
  | TypeAbsExprType abs ->
    let params = List.map (fun param -> param.param_type_name ^ ": " ^ display param.param_type) abs.type_abs_expr_type_params in
    "[" ^ (String.concat ", " params) ^ "] -> " ^ (display abs.type_abs_expr_type_body)
  | TypeTuple tuple ->
    let types = List.map display tuple.type_tuple_types in
    "(" ^ (String.concat ", " types) ^ ")"
  | TypeRecord record ->
    let attrs = List.map (fun (name, attr) -> name ^ ": " ^ display attr.attr_type) (List.of_seq (NameMap.to_seq record.type_record_attrs)) in
    "{" ^ (String.concat ", " attrs) ^ "}"
  | TypeInter inter ->
    (display inter.type_inter_left) ^ " & " ^ (display inter.type_inter_right)
  | TypeUnion union ->
    (display union.type_union_left) ^ " | " ^ (display union.type_union_right)
  | TypeAbs abs ->
    let params = List.map (fun param -> param.param_type_name ^ ": " ^ display param.param_type) abs.type_abs_params in
    "[" ^ (String.concat ", " params) ^ "] => " ^ (display abs.type_abs_body)
  | TypeApp app ->
    let args = List.map display app.type_app_args in
    (display app.type_app_type) ^ "[" ^ (String.concat ", " args) ^ "]"
