type error = {
  message: string;
  span: Code.span;
}

exception Error of error

let raise message span =
  raise (Error { message; span })

let validate_proper (type': Abt.type') =
  let span = Abt.type_span type' in
  let type' = Abt.display type' in
  raise
    ("expected proper type but found type `" ^ type' ^ "`")
    span

let validate_inter_kind (inter: Abt.type_inter) =
  let span = inter.span in
  let inter = Abt.display (TypeInter inter) in
  raise
    ("both operands of intersection `" ^ inter ^ "` must be of the same kind")
    span

let validate_union_kind (union: Abt.type_union) =
  let span = union.span in
  let union = Abt.display (TypeUnion union) in
  raise
    ("both operands of union `" ^ union ^ "` must be of the same kind")
    span

let validate_app_arg (app: Abt.type_app) (lower: Node.type') (upper: Node.type') (arg: Node.type') =
  let span = app.span in
  let lower = Display.display lower in
  let upper = Display.display upper in
  let arg = Display.display arg in
  raise
    ("expected type within interval ``" ^ lower ^ " .. " ^ upper ^ "` but found type `" ^ arg ^ "`")
    span

let validate_interval (interval: Abt.interval) lower upper =
  let span = interval.span in
  let lower = Display.display lower in
  let upper = Display.display upper in
  raise
    ("inconsistent type interval, expected the lower bound to be a subtype of the upper bound but found types `" ^ lower ^ "` and `" ^ upper ^ "`")
    span
