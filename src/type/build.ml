open Node
open Context

let bind name = { Abt.name }

let top = Top

let bot = Bot

let unit = Unit

let bool = Bool

let int = Int

let string = String

let var bind = Var { bind }

let tuple elems = Tuple { elems }

let record attrs =
  let attrs = attrs
    |> List.map (fun (label, type') -> label, { label; type' })
    |> List.to_seq
    |> Util.NameMap.of_seq in
  Record { attrs }

let lam param ret = Lam { param; ret }

let univ name lower upper ret =
  let bind = bind name in
  let param: param = { bind; lower; upper } in
  let ret = ret (var bind) in
  Univ { param; ret }

let univ_0 name ret = univ name bot top ret

let abs name lower upper body =
  let bind = bind name in
  let param: param = { bind; lower; upper } in
  let body = body (var bind) in
  Abs { param; body }

let abs_0 name body = abs name bot top body

let app abs arg = App { abs; arg }

let rec' name body =
  let bind = bind name in
  let body = body (var bind) in
  Rec { bind; body }

let union left right = Union { left; right }

let inter left right = Inter { left; right }

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
