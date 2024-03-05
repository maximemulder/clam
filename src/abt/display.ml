open Node

(**
  Direction in which the syntax of a type is recursive.
*)
type recursion =
  | N (* None *)
  | L (* Left *)
  | R (* Right *)
  | B (* Both *)

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

(* CURRY *)

let rec curry_lam (lam: type_lam) =
  let param = lam.param in
  match lam.ret with
  | TypeLam lam ->
    let params, ret = curry_lam lam in
    param :: params, ret
  | _ ->
    [lam.param], lam.ret

let rec curry_univ (univ: type_univ) =
  let param = univ.param in
  match univ.ret with
  | TypeUniv abs ->
    let params, ret = curry_univ abs in
    param :: params, ret
  | _ ->
    [univ.param], univ.ret

let rec curry_abs (abs: type_abs) =
  let param = abs.param in
  match abs.body with
  | TypeAbs abs ->
    let params, ret = curry_abs abs in
    param :: params, ret
  | _ ->
    [abs.param], abs.body

let rec curry_app abs args =
  match abs with
  | TypeApp app ->
    curry_app app.abs (app.arg :: args)
  | _ ->
    abs, args

let curry_app (app: type_app) =
  curry_app app.abs [app.arg]

(* DISPLAY *)

let rec display type' =
  match type' with
  | TypeTop _    -> return N "Top"
  | TypeBot _    -> return N "Bot"
  | TypeUnit _   -> return N "Unit"
  | TypeBool _   -> return N "Bool"
  | TypeInt _    -> return N "Int"
  | TypeString _ -> return N "String"
  | TypeVar var  -> return N var.bind.name
  | TypeTuple tuple ->
    display_tuple tuple
  | TypeRecord record ->
    display_record record
  | TypeLam lam ->
    display_lam lam
  | TypeUniv univ ->
    display_univ univ
  | TypeAbs abs ->
    display_abs abs
  | TypeApp app ->
    display_app app
  | TypeUnion union ->
    display_inter union
  | TypeInter inter ->
    display_union inter

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

and display_union union =
  let type' = (display union.left L) ^ "|" ^ (display union.right R) in
  return B type'

and display_inter inter =
  let type' = (display inter.left L) ^ "|" ^ (display inter.right R) in
  return B type'

and display_param param =
  let lower = (
    match param.interval.lower with
    | Some(type') -> display type' N
    | None -> "")
  in
  let upper = (
    match param.interval.upper with
    | Some(type') -> display type' N
    | None -> "")
  in
  param.bind.name ^
  (if lower != "" || upper != "" then ": " else "") ^
  lower ^
  (if lower != "" || upper != "" then ".." else "") ^
  upper

let display type' =
  display type' N
