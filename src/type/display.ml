open Node

(**
  Direction in which the syntax of a type is recursive.
*)
type recursion =
  | N (* None *)
  | L (* Left *)
  | R (* Right *)
  | B (* Both *)

let single (type': type') =
  match type' with
  | { dnf = [[base]] } ->
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

let rec curry_lam (lam: lam) =
  let param = lam.param in
  match single lam.ret with
  | Some (Lam lam) ->
    let params, ret = curry_lam lam in
    param :: params, ret
  | _ ->
    [lam.param], lam.ret

let rec curry_univ (univ: univ) =
  let param = univ.param in
  match single univ.ret with
  | Some (Univ univ) ->
    let params, ret = curry_univ univ in
    param :: params, ret
  | _ ->
    [univ.param], univ.ret

let rec curry_abs (abs: abs) =
  let param = abs.param in
  match single abs.body with
  | Some (Abs abs) ->
    let params, ret = curry_abs abs in
    param :: params, ret
  | _ ->
    [abs.param], abs.body

let rec curry_app (abs: type') args =
  match single abs with
  | Some (App app) ->
    curry_app app.abs (app.arg :: args)
  | _ ->
    abs, args

let curry_app (app: app) =
  curry_app app.abs [app.arg]

let rec display (type': type') =
  display_union type'.dnf

and display_union types parent =
  let self, elem = group_types types parent in
  let types = List.map (Util.flip display_inter elem) types in
  let types = String.concat " | " types in
  return self types parent

and display_inter types parent =
  let self, elem = group_types types parent in
  let types = List.map (Util.flip display_base elem) types in
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
  | Lam lam ->
    display_lam lam
  | Univ univ ->
    display_univ univ
  | Abs abs ->
    display_abs abs
  | App app ->
    display_app app

and display_tuple tuple =
  let types = List.map (Util.flip display N) tuple.elems in
  return N ("{" ^ (String.concat ", " types) ^ "}")

and display_record record =
  let attrs = List.of_seq (Util.NameMap.to_seq record.attrs)
    |> List.map snd
    |> List.map display_record_attr
  in
  return N ("{" ^ (String.concat ", " attrs) ^ "}")

and display_record_attr attr =
  attr.label ^ ": " ^ display attr.type' N

and display_lam lam =
  let params, ret = curry_lam lam in
  let params = List.map (Util.flip display N) params in
  let type' = "(" ^ (String.concat ", " params) ^ ") -> " ^ (display ret R) in
  return R type'

and display_univ univ =
  let params, ret = curry_univ univ in
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
  let args = List.map (Util.flip display N) args in
  let type' = (display abs L) ^ "[" ^ (String.concat ", " args) ^ "]" in
  return L type'

and display_param param =
  param.bind.name ^
  let lower = Compare.compare param.lower Node.bot |> not in
  let upper = Compare.compare param.upper Node.top |> not in
  (if lower || upper then ": " else "") ^
  (if lower then display param.lower N ^ " " else "") ^
  (if lower || upper then ".." else "") ^
  (if upper then " " ^ display param.upper N else "")

let display type' =
  display type' N

let display_base type' =
  display_base type' N
