let rec display (type': Model.type') =
  match type' with
  | TypeTop _    -> "Top"
  | TypeBot _    -> "Bot"
  | TypeUnit _   -> "Unit"
  | TypeBool _   -> "Bool"
  | TypeInt _    -> "Int"
  | TypeChar _   -> "Char"
  | TypeString _ -> "String"
  | TypeVar var -> var.param.name
  | TypeAbsExpr abs ->
    let params = List.map display abs.params in
    "(" ^ (String.concat ", " params) ^ ") -> " ^ (display abs.body)
  | TypeAbsExprType abs ->
    let params = List.map display_param abs.params in
    "[" ^ (String.concat ", " params) ^ "] -> " ^ (display abs.body)
  | TypeTuple tuple ->
    let types = List.map display tuple.elems in
    "(" ^ (String.concat ", " types) ^ ")"
  | TypeRecord record ->
    let attrs = List.map display_attr_entry (List.of_seq (Utils.NameMap.to_seq record.attrs)) in
    "{" ^ (String.concat ", " attrs) ^ "}"
  | TypeInter inter ->
    "(" ^ (display inter.left) ^ " & " ^ (display inter.right) ^ ")"
  | TypeUnion union ->
    "(" ^ (display union.left) ^ " | " ^ (display union.right) ^ ")"
  | TypeAbs abs ->
    let params = List.map display_param abs.params in
    "[" ^ (String.concat ", " params) ^ "] => " ^ (display abs.body)
  | TypeApp app ->
    let args = List.map display app.args in
    (display app.type') ^ "[" ^ (String.concat ", " args) ^ "]"

and display_attr_entry (name, attr) =
  name ^ ": " ^ display attr.type'

and display_param param =
  param.name ^ ": " ^ display param.type'
