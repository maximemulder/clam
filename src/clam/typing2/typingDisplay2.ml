
let surround string surround =
  if surround then
    "(" ^ string ^ ")"
  else
    string

let rec display type' surr =
  display_union type' surr

and display_union (union: Type.union) surr =
  let surr_inner = List.length union.union <> 1 in
  let types = List.map (Utils.flip display_inter surr_inner) union.union in
  let types = String.concat " | " types in
  surround types surr

and display_inter (inter: Type.inter) surr =
  let surr_inner = List.length inter.inter <> 1 in
  let types = List.map (Utils.flip display_base surr_inner) inter.inter in
  let types = String.concat " | " types in
  surround types surr

and display_base (type': Type.base) surr =
  match type' with
  | Top    _ -> "Top"
  | Bot    _ -> "Bot"
  | Unit   _ -> "Unit"
  | Bool   _ -> "Bool"
  | Int    _ -> "Int"
  | Char   _ -> "Char"
  | String _ -> "String"
  | Var var -> var.bind.name
  | Tuple tuple ->
    let types = List.map (Utils.flip display false) tuple.elems in
    "(" ^ (String.concat ", " types) ^ ")"
  | Record record ->
    let attrs = List.map display_attr_entry (List.of_seq (Utils.NameMap.to_seq record.attrs)) in
    "{" ^ (String.concat ", " attrs) ^ "}"
  | AbsExpr abs ->
    let type' = "(" ^ (display abs.param false) ^ ") -> " ^ (display abs.ret true) in
    surround type' surr
  | AbsTypeExpr abs ->
    "[" ^ (display_param abs.param) ^ "] -> " ^ (display abs.ret true)
  | Abs abs ->
    let type' = "[" ^ (display_param abs.param) ^ "] => " ^ (display abs.body true) in
    surround type' surr
  | App app ->
    let type' = (display app.abs true) ^ "[" ^ (display app.arg false) ^ "]" in
    surround type' surr

and display_attr_entry (_, attr) =
  attr.name ^ ": " ^ display attr.type' false

and display_param param =
  param.bind.name ^ ": " ^ display param.bound false

let display type' =
  display type' false
