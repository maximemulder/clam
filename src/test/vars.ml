open Type.Context

let inline v _ = v

let id v = v

(* Types *)

include Type

(* TODO: Adopt new bind system once typing is refactored *)

let bind name =
  { Abt.name }

let record attrs =
  let attrs = attrs
    |> List.map (fun (label, type') -> (label, { label; type' }))
    |> List.to_seq
    |> Util.NameMap.of_seq in
  base (Record { attrs })

let univ name bound ret =
  let bind = bind name in
  let param: Type.param = { bind; lower = bot; upper = bound } in
  let ret = ret (var bind) in
  base (Univ { param; ret })

let abs name bound body =
  let bind = bind name in
  let param: Type.param = { bind; lower = bot; upper = bound } in
  let body = body (var bind) in
  base (Abs { param; body })

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
    { bind = a; lower = bot; upper = top };
    { bind = b; lower = bot; upper = top };
    { bind = c; lower = bot; upper = top };
    { bind = d; lower = bot; upper = top };
    { bind = e; lower = bot; upper = top };
    { bind = f; lower = bot; upper = top };
    { bind = z; lower = bot; upper = top };
    { bind = ea; lower = bot; upper = var a };
    { bind = fa; lower = bot; upper = var a };
  ]
}

let union types =
  Util.list_reduce (System.join ctx) types

let inter types =
  Util.list_reduce (System.meet ctx) types

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

(*
  TODO: Although the tests have been ported to the new archtitecture, they haven't
  yet be updated to better match the new type structure. This work should eventually
  be done, with notably union and inter no longer using join and meet.

  union (p: union list):
    each p must have a single intersection

  inter (p: union list)
    p must have a single intersection and base
*)
