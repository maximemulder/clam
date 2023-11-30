open Clam.Model

let pos = prim_pos

let top    = prim_top
let bot    = prim_bot
let unit   = prim_unit
let bool   = prim_bool
let int    = prim_int
let char   = prim_char
let string = prim_string

let inline type' _ = type'

let id type' = type'

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
let z = var "Z" top

let ea = var "E" a
let fa = var "F" a

let with_var (name, type') body =
  body (var name type')
