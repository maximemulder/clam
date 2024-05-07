open Context
open Context.Monad
open Node

type kind =
  | Proper
  | Higher of higher

and higher = {
  lower: type';
  upper: type';
  body: kind;
}

let rec get_kind type' =
  match type' with
  | Union union ->
    get_kind_union union
  | Inter inter ->
    get_kind_inter inter
  | Var var ->
    get_kind_var var
  | Abs abs ->
    get_kind_abs abs
  | App app ->
    get_kind_app app
  | _ ->
    return Proper

and get_kind_union union =
  (* Both left and right have the same kind in a well-formed type. *)
  get_kind union.left

and get_kind_inter inter =
  (* Both left and right have the same kind in a well-formed type. *)
  get_kind inter.left

and get_kind_var var =
  let* var = get_var var.bind in
  match var with
  | Fresh _ ->
    return Proper
  | Rigid rigid ->
    (* Both the lower and upper bounds have the same kind in a well-formed
      type. *)
    get_kind rigid.lower

and get_kind_abs abs =
  let* body = with_param_rigid abs.param (get_kind abs.body) in
  return (Higher { lower = abs.param.lower; upper = abs.param.upper; body })

and get_kind_app app =
  let* abs = get_kind app.abs in
  match abs with
  | Higher { body; _ } ->
    return body
  | Proper ->
    failwith "Ill-formed type application in `Kind.get_kind_app`."

let rec get_kind_min kind =
  match kind with
  | Proper ->
    Bot
  | Higher { lower; upper; body } ->
    let bind: Abt.bind_type = { name = "_" } in
    let param: Node.param = { bind; lower; upper } in
    Abs { param; body = get_kind_min body }

let rec get_kind_max kind =
  match kind with
  | Proper ->
    Top
  | Higher { lower; upper; body } ->
    let bind: Abt.bind_type = { name = "_" } in
    let param: Node.param = { bind; lower; upper } in
    Abs { param; body = get_kind_max body }

let is_proper type' =
  let* kind = get_kind type' in
  return (kind = Proper)
