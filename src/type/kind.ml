open Node
open Context2
open Context2.Monad

type kind =
  | Type
  | Abs of abs

and abs = {
  lower: type';
  upper: type';
  body: kind;
}

let rec get_kind type' =
  get_kind_base (List.nth (List.nth type'.dnf 0) 0)

and get_kind_base type' =
  match type' with
  | Var var ->
    get_kind_var var
  | Abs abs ->
    get_kind_abs abs
  | App app ->
    get_kind_app app
  | _ ->
    return Type

and get_kind_var var =
  let* var = get_var var.bind in
  match var with
  | Fresh _ ->
    return Type
  | Rigid rigid ->
    get_kind rigid.lower

and get_kind_abs abs =
  let* body = with_param_rigid abs.param (get_kind abs.body) in
  return (Abs { lower = abs.param.lower; upper = abs.param.upper; body })

and get_kind_app app =
  let* abs = get_kind app.abs in
  match abs with
  | Abs { body; _ } ->
    return body
  | Type ->
    invalid_arg "TypeKind.get_kind"

let rec get_kind_min kind =
  match kind with
  | Type ->
    Node.bot
  | Abs { lower; upper; body } ->
    let bind: Abt.bind_type = { name = "_" } in
    let param: Node.param = { bind; lower; upper } in
    Node.abs param (get_kind_min body)

let rec get_kind_max kind =
  match kind with
  | Type ->
    Node.top
  | Abs { lower; upper; body } ->
    let bind: Abt.bind_type = { name = "_" } in
    let param: Node.param = { bind; lower; upper } in
    Node.abs param (get_kind_max body)

let rec display kind =
  match kind with
  | Type -> "*"
  | Abs abs -> "[" ^ Display.display abs.lower ^ " .. " ^ Display.display abs.upper ^ "] -> " ^ display abs.body
