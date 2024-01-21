(*
  Direction in which the syntax of a type is recursive.
*)
type recursion =
  | N (* None *)
  | L (* Left *)
  | R (* Right *)
  | B (* Both *)

let single (type': Type.type') =
  match type' with
  | { union = [{ inter = [base] }] } ->
    Some base
  | _ ->
    None

let return self string parent =
  let cond = match self, parent with
  | N, _ -> false
  | _, N -> false
  | L, L -> false
  | R, R -> false
  | _, _ -> true
  in
  if cond then
    "(" ^ string ^ ")"
  else
    string

let group_types types parent =
  if List.length types <> 1 then
    B, B
  else
    N, parent

let rec curry_abs_expr (abs: Type.abs_expr) =
  let param = abs.param in
  match single abs.ret with
  | Some (Type.AbsExpr abs) ->
    let params, ret = curry_abs_expr abs in
    param :: params, ret
  | _ ->
    [abs.param], abs.ret

let rec curry_abs_type_expr (abs: Type.abs_type_expr) =
  let param = abs.param in
  match single abs.ret with
  | Some (Type.AbsTypeExpr abs) ->
    let params, ret = curry_abs_type_expr abs in
    param :: params, ret
  | _ ->
    [abs.param], abs.ret

let rec curry_abs (abs: Type.abs) =
  let param = abs.param in
  match single abs.body with
  | Some (Type.Abs abs) ->
    let params, ret = curry_abs abs in
    param :: params, ret
  | _ ->
    [abs.param], abs.body

let rec curry_app (abs: Type.type') args =
  match single abs with
  | Some (Type.App app) ->
    curry_app app.abs (app.arg :: args)
  | _ ->
    abs, args

let curry_app (app: Type.app) =
  curry_app app.abs [app.arg]

let rec display (type': Type.type') =
  display_union type'

and display_union union parent =
  let self, elem = group_types union.union parent in
  let types = List.map (Utils.flip display_inter elem) union.union in
  let types = String.concat " | " types in
  return self types parent

and display_inter inter parent =
  let self, elem = group_types inter.inter parent in
  let types = List.map (Utils.flip display_base elem) inter.inter in
  let types = String.concat " & " types in
  return self types parent

and display_base type' =
  match type' with
  | Top     -> return N "Top"
  | Bot     -> return N "Bot"
  | Unit    -> return N "Unit"
  | Bool    -> return N "Bool"
  | Int     -> return N "Int"
  | String  -> return N "String"
  | Var var -> return N var.bind.name
  | Tuple tuple ->
    display_tuple tuple
  | Record record ->
    display_record record
  | AbsExpr abs ->
    display_abs_expr abs
  | AbsTypeExpr abs ->
    display_abs_type_expr abs
  | Abs abs ->
    display_abs abs
  | App app ->
    display_app app

and display_tuple tuple =
  let types = List.map (Utils.flip display N) tuple.elems in
  return N ("{" ^ (String.concat ", " types) ^ "}")

and display_record record =
  let attrs = List.of_seq (Utils.NameMap.to_seq record.attrs)
    |> List.map snd
    |> List.map display_record_attr
  in
  return N ("{" ^ (String.concat ", " attrs) ^ "}")

and display_record_attr attr =
  attr.name ^ ": " ^ display attr.type' N

and display_abs_expr abs =
  let params, ret = curry_abs_expr abs in
  let params = List.map (Utils.flip display N) params in
  let type' = "(" ^ (String.concat ", " params) ^ ") -> " ^ (display ret R) in
  return R type'

and display_abs_type_expr abs =
  let params, ret = curry_abs_type_expr abs in
  let params = List.map display_param params in
  let type' = "[" ^ (String.concat ", " params) ^ "] -> " ^ (display ret R) in
  return R type'

and display_abs abs =
  let params, body = curry_abs abs in
  let params = List.map display_param params in
  let type' = "[" ^ (String.concat ", " params) ^ "] => " ^ (display body R) in
  return R type'

and display_app app =
  let abs, args = curry_app app in
  let args = List.map (Utils.flip display N) args in
  let type' = (display abs L) ^ "[" ^ (String.concat ", " args) ^ "]" in
  return L type'

and display_param param =
  let bound = display param.bound N in
  param.bind.name ^ ": " ^ bound

let display type' =
  display type' N

let display_base type' =
  display_base type' N

let display_context_entry (entry: TypeContext.entry) =
    entry.bind.name ^ " <: " ^ display entry.bound

let display_context (ctx: TypeContext.context) =
  let entries = List.map display_context_entry ctx.assumptions in
  String.concat ", " entries
