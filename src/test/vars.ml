open Clam.Model
open Clam.Primitive

let inline v _ = v

let id v = v

(* Types *)

let top    = top
let bot    = bot
let unit   = unit
let bool   = bool
let int    = int
let char   = char
let string = string

let var name bound =
  TypeVar { pos; param = { name; bound }}

let tuple elems =
  TypeTuple { pos; elems }

let record attrs =
  let attrs = attrs
    |> List.map (fun (name, type') -> (name, { pos; name; type' }))
    |> List.to_seq
    |> Clam.Utils.NameMap.of_seq in
  TypeRecord { pos; attrs }

let union left right =
  TypeUnion { pos; left; right }

let inter left right =
  TypeInter { pos; left; right }

let abs_expr param body =
  TypeAbsExpr { pos; param; body }

let abs_expr_type (name, bound) body =
  let param = { name; bound } in
  let body = body (TypeVar { pos; param }) in
  TypeAbsExprType { pos; param; body }

let abs name bound body =
  let param = { name; bound } in
  let body = body (TypeVar { pos; param }) in
  TypeAbs { pos; param; body }

let app type' arg =
  TypeApp { pos; type'; arg }

let a = var "A" top
let b = var "B" top
let c = var "C" top
let d = var "D" top
let e = var "E" top
let f = var "F" top
let z = var "Z" top

let ea = var "E" a
let fa = var "F" a

let with_var (name, type') body =
  body (var name type')

(* Expressions *)

let e_unit =
  ExprUnit { pos }

let e_bool value =
  ExprBool { pos; value }

let e_int value =
  ExprInt { pos; value }

let e_string value =
  ExprString { pos; value }

let e_tuple elems =
  ExprTuple { pos; elems }

let e_record attrs =
  let attrs = List.map (fun (name, expr) -> ({ pos; name; expr })) attrs in
  ExprRecord { pos; attrs }

let e_ascr expr type' =
  ExprAscr { pos; expr; type' }

let e_if cond then' else' =
  ExprIf { pos; cond; then'; else' }

let e_abs (name, type') body =
  let param = { pos; id = 0; name; type' = Some type' } in
  let body = body param in
  ExprAbs { pos; param; body }

let e_app expr arg =
  ExprApp { pos; expr; arg }

let e_abs_te (name, bound) body =
  let param = { name; bound } in
  let body = body param in
  ExprTypeAbs { pos; param; body }

let e_app_te expr arg =
  ExprTypeApp { pos; expr; arg }
