open Node

type kind =
  | Type
  | Abs of abs

and abs = {
  lower: type';
  upper: type';
  ret: kind;
}

let rec get_kind ctx type' =
  get_kind_union ctx type'

and get_kind_union ctx union =
  get_kind_inter ctx (List.nth union.union 0)

and get_kind_inter ctx inter =
  get_kind_base ctx (List.nth inter.inter 0)

and get_kind_base ctx type' =
  match type' with
  | Var var ->
    let lower, _ = Context.get_bounds ctx var.bind in
    get_kind ctx lower
  | Abs abs ->
    let ctx = Context.add_param ctx abs.param in
    let ret  = get_kind ctx abs.body in
    Abs { lower = abs.param.lower; upper = abs.param.upper; ret }
  | App app ->
    get_kind_app ctx app
  | _ ->
    Type

and get_kind_app ctx app =
  match get_kind ctx app.abs with
  | Abs { ret; _ } ->
    ret
  | Type ->
    invalid_arg "TypeKind.get_kind"

let rec get_kind_max ctx kind =
  match kind with
  | Type ->
    Node.top
  | Abs { lower; upper; ret } ->
    let bind: Abt.bind_type = { name = "_" } in
    let param: Node.param = { bind; lower; upper } in
    Node.abs param (get_kind_max ctx ret)

let rec get_kind_min ctx kind =
  match kind with
  | Type ->
    Node.bot
  | Abs { lower; upper; ret } ->
    let bind: Abt.bind_type = { name = "_" } in
    let param: Node.param = { bind; lower; upper } in
    Node.abs param (get_kind_min ctx ret)

let rec display kind =
  match kind with
  | Type -> "*"
  | Abs abs -> "[" ^ Display.display abs.lower ^ " .. " ^ Display.display abs.upper ^ "] -> " ^ display abs.ret
