let group string grp =
  if grp then
    "(" ^ string ^ ")"
  else
    string

let rec display (type': Type.type') grp =
  display_union type' grp

and display_union union grp =
  let grp_self = grp && List.length union.union <> 1 in
  let grp_elem = grp || List.length union.union <> 1 in
  let types = List.map (Utils.flip display_inter grp_elem) union.union in
  let types = String.concat " | " types in
  group types grp_self

and display_inter inter grp =
  let grp_self = grp && List.length inter.inter <> 1 in
  let grp_elem = grp || List.length inter.inter <> 1 in
  let types = List.map (Utils.flip display_base grp_elem) inter.inter in
  let types = String.concat " & " types in
  group types grp_self

and display_base (type': Type.base) grp =
  match type' with
  | Top    -> "Top"
  | Bot    -> "Bot"
  | Unit   -> "Unit"
  | Bool   -> "Bool"
  | Int    -> "Int"
  | Char   -> "Char"
  | String -> "String"
  | Var var -> var.bind.name
  | Tuple tuple ->
    let types = List.map (Utils.flip display false) tuple.elems in
    "(" ^ (String.concat ", " types) ^ ")"
  | Record record ->
    let attrs = List.map display_attr_entry (List.of_seq (Utils.NameMap.to_seq record.attrs)) in
    "{" ^ (String.concat ", " attrs) ^ "}"
  | AbsExpr abs ->
    let type' = "(" ^ (display abs.param false) ^ ") -> " ^ (display abs.ret true) in
    group type' grp
  | AbsTypeExpr abs ->
    let type' = "[" ^ (display_param abs.param) ^ "] -> " ^ (display abs.ret true) in
    group type' grp
  | Abs abs ->
    let type' = "[" ^ (display_param abs.param) ^ "] => " ^ (display abs.body true) in
    group type' grp
  | App app ->
    let type' = (display app.abs true) ^ "[" ^ (display app.arg false) ^ "]" in
    group type' grp

and display_attr_entry (_, attr) =
  attr.name ^ ": " ^ display attr.type' false

and display_param param =
  param.bind.name ^ ": " ^ display param.bound false

let display type' =
  display type' false

let display_base type' =
  display_base type' false

let display_context_entry (entry: TypeContext.entry) =
    entry.bind.name ^ " <: " ^ display entry.bound

let display_context (ctx: TypeContext.context) =
  let entries = List.map display_context_entry ctx.assumptions in
  String.concat ", " entries
