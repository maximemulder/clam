open Context2
open Context2.Monad
open Node

let return_base type' = return (base type')

let rec validate_proper type' =
  let* type'' = validate type' in
  let* kind = Kind.get_kind type'' in
  if kind <> Type then
    Error.validate_proper type'
  else
    return type''

and validate (type': Abt.type') =
  match type' with
  | TypeTop    _ ->
    return_base Top
  | TypeBot    _ ->
    return_base Bot
  | TypeUnit   _ ->
    return_base Unit
  | TypeBool   _ ->
    return_base Bool
  | TypeInt    _ ->
    return_base Int
  | TypeString _ ->
    return_base String
  | TypeVar var ->
    return_base (Var { bind = var.bind })
  | TypeTuple tuple ->
    validate_tuple tuple
  | TypeRecord record ->
    validate_record record
  | TypeLam lam ->
    validate_lam lam
  | TypeUniv univ ->
    validate_univ univ
  | TypeAbs abs ->
    validate_abs abs
  | TypeApp app ->
    validate_app app
  | TypeUnion union ->
    validate_union union
  | TypeInter inter ->
    validate_inter inter

and validate_tuple tuple =
  let* elems = list_map validate_proper tuple.elems in
  return_base (Tuple { elems })

and validate_record record =
  let* attrs = map_map validate_record_attr record.attrs in
  return_base (Record { attrs })

and validate_record_attr attr =
  let* type' = validate_proper attr.type' in
  return { label = attr.label; type' }

and validate_lam lam =
  let* param = validate_proper lam.param in
  let* ret   = validate_proper lam.ret   in
  return_base (Lam { param; ret })

and validate_univ univ =
  let* param, ret = validate_param_with univ.param (validate_proper univ.ret) in
  return_base (Univ { param; ret })

and validate_abs abs =
  let* param, body = validate_param_with abs.param (validate abs.body) in
  return_base (Abs { param; body })

(* TODO: Handle lower bounds *)
and validate_app app =
  let* abs = validate app.abs in
  let* arg = validate app.arg in
  let* param = validate_app_param abs in
  let* cond = System2.isa arg param in
  if not cond then
    Error.validate_app_arg app param arg
  else
  System2.compute abs arg

and validate_union union =
  let* left  = validate union.left  in
  let* right = validate union.right in
  let* kind_left  = Kind.get_kind left in
  let* kind_right = Kind.get_kind right in
  let* cond = System2.is_kind kind_left kind_right in
  if not cond then
    Error.validate_union_kind union
  else
  System2.join left right

and validate_inter inter =
  let* left  = validate inter.left  in
  let* right = validate inter.right in
  let* kind_left  = Kind.get_kind left in
  let* kind_right = Kind.get_kind right in
  let* cond = System2.is_kind kind_left kind_right in
  if not cond then
    Error.validate_inter_kind inter
  else
  System2.meet left right

(* TODO: Handle both lower and upper bounds in parameter validation *)
and validate_app_param abs =
  validate_app_param_union abs.dnf

and validate_app_param_union types =
  let* types = list_map validate_app_param_inter types in
  list_reduce System2.meet types

and validate_app_param_inter types =
  let* types = list_map validate_app_param_base types in
  list_reduce System2.join types

and validate_app_param_base type' =
  match type' with
  | Var var ->
    let* var = get_var var.bind in
    (match var with
    | Fresh fresh ->
      validate_app_param fresh.upper
    | Rigid rigid ->
      validate_app_param rigid.upper)
  | Abs abs ->
    return abs.param.upper
  | _ ->
    invalid_arg "validate_app_param_base"

and validate_param (param: Abt.param_type) =
  let* lower, upper = match param.interval.lower, param.interval.upper with
  | Some lower, Some upper ->
    let* lower = validate lower in
    let* upper = validate upper in
    return (lower, upper)
  | Some lower, None ->
    let* lower = validate lower in
    let* kind = Kind.get_kind lower in
    let upper = Kind.get_kind_max kind in
    return (lower, upper)
  | None, Some upper ->
    let* upper = validate upper in
    let* kind = Kind.get_kind upper in
    let lower = Kind.get_kind_min kind in
    return (lower, upper)
  | None, None ->
    return (Node.bot, Node.top)
  in
  let* cond = System2.isa lower upper in
  if not cond then
    Error.validate_interval param.interval lower upper
  else
  return { bind = param.bind; lower; upper }

and validate_param_with param f =
  let* param = validate_param param in
  let* type' = with_param_rigid param f in
  return (param, type')
