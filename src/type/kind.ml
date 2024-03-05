type kind =
  | Type
  | Abs of kind * kind

let rec get_kind ctx (type': Node.type') =
  get_kind_union ctx type'

and get_kind_union ctx (union: Node.union) =
  get_kind_inter ctx (List.nth union.union 0)

and get_kind_inter ctx (inter: Node.inter) =
  get_kind_base ctx (List.nth inter.inter 0)

and get_kind_base ctx (type': Node.base) =
  match type' with
  | Var var ->
    let _, upper = Context.get_bounds ctx var.bind in
    get_kind ctx upper
  | Abs abs ->
    let param = get_kind ctx abs.param.lower in
    let ctx = Context.add_param ctx abs.param in
    let body  = get_kind ctx abs.body in
    Abs (param, body)
  | App app ->
    get_kind_app ctx app
  | _ ->
    Type

and get_kind_app ctx app =
  match get_kind ctx app.abs with
  | Abs (_, body) ->
    body
  | Type ->
    invalid_arg "TypeKind.get_kind"

(* TODO: These two functions do not work when type abstractions have non-extremal bounds.
  Since type abstractions are invariant with regards to their parameter, an extremal type is
  only so with regards to some parameters, which should be added to a kind. *)
let rec get_top ctx kind =
  match kind with
  | Type ->
    Node.top
  | Abs (param, kind) ->
    let bind: Abt.bind_type = { name = "_" } in
    let param: Node.param = { bind; lower = (get_bot ctx param); upper = (get_top ctx param)} in
    Node.abs param (get_bot ctx kind)

and get_bot ctx kind =
  match kind with
  | Type ->
    Node.bot
  | Abs (param, kind) ->
    let bind: Abt.bind_type = { name = "_" } in
    let param: Node.param = { bind; lower = (get_bot ctx param); upper = (get_top ctx param)} in
    Node.abs param (get_bot ctx kind)
