open Clam
open Type
open TypeContext

let inline v _ = v

let id v = v

(* Types *)

include TypePrimitive

(* TODO: Adopt new bind system once typing is refactored *)

let bind name =
  { Model.name }

let var bind =
  base (Var { bind })

let tuple elems =
  base (Tuple { elems })

let record attrs =
  let attrs = attrs
    |> List.map (fun (name, type') -> (name, { name; type' }))
    |> List.to_seq
    |> Utils.NameMap.of_seq in
  base (Record { attrs })

let abs_expr param ret =
  base (AbsExpr { param; ret })

let abs_expr_type (name, bound) ret =
  let bind = bind name in
  let param: Type.param = { bind; bound } in
  let ret = ret (var bind) in
  base (AbsTypeExpr { param; ret })

let abs name bound body =
  let bind = bind name in
  let param: Type.param = { bind; bound } in
  let body = body (var bind) in
  base (Abs { param; body })

let app abs arg =
  base (App { abs; arg })

let a = bind "A"
let b = bind "B"
let c = bind "C"
let d = bind "D"
let e = bind "E"
let f = bind "F"
let z = bind "Z"

let ea = bind "EA"
let fa = bind "FA"

let ctx = {
  assumptions = [
    { bind = a; bound = top };
    { bind = b; bound = top };
    { bind = c; bound = top };
    { bind = d; bound = top };
    { bind = e; bound = top };
    { bind = f; bound = top };
    { bind = z; bound = top };
    { bind = ea; bound = var a };
    { bind = fa; bound = var a };
  ]
}

let union types =
  Utils.list_reduce (Typing2.join ctx) types

let inter types =
  Utils.list_reduce (Typing2.meet ctx) types

let a = var a
let b = var b
let c = var c
let d = var d
let e = var e
let f = var f
let z = var z
let ea = var ea
let fa = var fa

(* Expressions *)

(*
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
  let bind = { name } in
  let param = { bind; bound } in
  let body = body param in
  ExprTypeAbs { pos; param; body }

let e_app_te expr arg =
  ExprTypeApp { pos; expr; arg }*)
