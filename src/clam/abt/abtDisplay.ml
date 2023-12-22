let add_surround string surround =
  if surround then
    "(" ^ string ^ ")"
  else
    string

let rec display type' =
  display_surround type' false

and display_surround (type': Abt.type') (surround: bool) =
  match type' with
  | TypeTop _    -> "Top"
  | TypeBot _    -> "Bot"
  | TypeUnit _   -> "Unit"
  | TypeBool _   -> "Bool"
  | TypeInt _    -> "Int"
  | TypeChar _   -> "Char"
  | TypeString _ -> "String"
  | TypeVar var -> var.bind.name
  | TypeAbsExpr abs ->
    let type' = "(" ^ (display abs.param) ^ ") -> " ^ (display_surround abs.body true) in
    add_surround type' surround
  | TypeAbsExprType abs ->
    "[" ^ (display_param abs.param) ^ "] -> " ^ (display_surround abs.body true)
  | TypeTuple tuple ->
    let types = List.map display tuple.elems in
    "(" ^ (String.concat ", " types) ^ ")"
  | TypeRecord record ->
    let attrs = List.map display_attr_entry (List.of_seq (Utils.NameMap.to_seq record.attrs)) in
    "{" ^ (String.concat ", " attrs) ^ "}"
  | TypeInter inter ->
    let type' = (display_surround inter.left true) ^ " & " ^ (display_surround inter.right true) in
    add_surround type' surround
  | TypeUnion union ->
    let type' = (display_surround union.left true) ^ " | " ^ (display_surround union.right true) in
    add_surround type' surround
  | TypeAbs abs ->
    let type' = "[" ^ (display_param abs.param) ^ "] => " ^ (display_surround abs.body true) in
    add_surround type' surround
  | TypeApp app ->
    let type' = (display_surround app.type' true) ^ "[" ^ (display app.arg) ^ "]" in
    add_surround type' surround

and display_attr_entry (name, attr) =
  name ^ ": " ^ display attr.type'

and display_param param =
  param.bind.name ^ ": " ^ display param.bound
