open Abt.Type
open Context

let span = Code.span_primitive

let bind name = { name }

let top    = Top    { span }
let bot    = Bot    { span }
let unit   = Unit   { span }
let bool   = Bool   { span }
let int    = Int    { span }
let string = String { span }

let var bind = Var { span; bind }

let tuple elems = Tuple { span; elems }

let record attrs =
  let attrs = attrs
    |> List.map (fun (label, type') -> label, { span; label; type' })
    |> List.to_seq
    |> Util.NameMap.of_seq in
  Record { span; attrs }

let lam param ret = Lam { span; param; ret }

let univ name lower upper ret =
  let bind = bind name in
  let param: param = { span; bind; lower; upper } in
  let ret = ret (var bind) in
  Univ { span; param; ret }

let univ_0 name ret = univ name bot top ret

let abs name lower upper body =
  let bind = bind name in
  let param: param = { span; bind; lower; upper } in
  let body = body (var bind) in
  Abs { span; param; body }

let abs_0 name body = abs name bot top body

let app abs arg = App { span; abs; arg }

let rec' name body =
  let bind = bind name in
  let body = body (var bind) in
  Rec { span; bind; body }

let union left right = Union { span; left; right }

let inter left right = Inter { span; left; right }

let ctx = empty

module Default = struct
  let a  = bind "A"
  let b  = bind "B"
  let c  = bind "C"
  let d  = bind "D"
  let e  = bind "E"
  let a1 = bind "A1"
  let a2 = bind "A2"
  let z  = bind "Z"

  let ctx = { ctx with rigids = [
    { bind = a;  lower = bot; upper = top };
    { bind = b;  lower = bot; upper = top };
    { bind = c;  lower = bot; upper = top };
    { bind = d;  lower = bot; upper = top };
    { bind = e;  lower = bot; upper = top };
    { bind = a1; lower = bot; upper = var a };
    { bind = a2; lower = bot; upper = var a };
    { bind = z;  lower = bot; upper = top };
  ]}

  let a = var a
  let b = var b
  let c = var c
  let d = var d
  let e = var e
  let a1 = var a1
  let a2 = var a2
  let z = var z
end
