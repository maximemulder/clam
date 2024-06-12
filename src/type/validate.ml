open Context
open Context.Monad
open Node
open System

let rec validate_proper type' =
  let* type'' = validate type' in
  let* cond = is_proper type'' in
  if not cond then
    Error.validate_proper type'
  else
    return type''

and validate (type': Abt.type') =
  print_endline ("AAA " ^ Abt.display type');
  match type' with
  | TypeTop    _ ->
    return Top
  | TypeBot    _ ->
    return Bot
  | TypeUnit   _ ->
    return Unit
  | TypeBool   _ ->
    return Bool
  | TypeInt    _ ->
    return Int
  | TypeString _ ->
    return String
  | TypeVar var ->
    return (Var { bind = var.bind })
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
  | TypeRec rec' ->
    print_endline "AA";
    let* a = validate_rec rec' in
    print_endline "BB";
    return a
  | TypeUnion union ->
    validate_union union
  | TypeInter inter ->
    validate_inter inter

and validate_tuple tuple =
  let* elems = list_map validate_proper tuple.elems in
  return (Tuple { elems })

and validate_record record =
  let* attrs = map_map validate_record_attr record.attrs in
  return (Record { attrs })

and validate_record_attr attr =
  let* type' = validate_proper attr.type' in
  return { label = attr.label; type' }

and validate_lam lam =
  let* param = validate_proper lam.param in
  let* ret   = validate_proper lam.ret   in
  return (Lam { param; ret })

and validate_univ univ =
  let* param, ret = validate_param_with univ.param (validate_proper univ.ret) in
  return (Univ { param; ret })

and validate_abs abs =
  let* param, body = validate_param_with abs.param (validate abs.body) in
  return (Abs { param; body })

and validate_app app =
  let* abs = validate app.abs in
  let* arg = validate app.arg in
  let* lower, upper = validate_app_param abs in
  let* lower_cond = System.isa lower arg in
  let* upper_cond = System.isa arg upper in
  if not lower_cond || not upper_cond then
    Error.validate_app_arg app lower upper arg
  else
  System.compute abs arg

and validate_rec rec' =
  with_def rec'.bind ((Var { bind = rec'.bind })) (validate rec'.body)

and validate_union union =
  let* left  = validate union.left  in
  let* right = validate union.right in
  let* kind_left  = Kind.get_kind left  in
  let* kind_right = Kind.get_kind right in
  let* cond = System.is_kind kind_left kind_right in
  if not cond then
    Error.validate_union_kind union
  else
  System.join left right

and validate_inter inter =
  let* left  = validate inter.left  in
  let* right = validate inter.right in
  let* kind_left  = Kind.get_kind left  in
  let* kind_right = Kind.get_kind right in
  let* cond = System.is_kind kind_left kind_right in
  if not cond then
    Error.validate_inter_kind inter
  else
  System.meet left right

and validate_app_param abs =
  match abs with
  | Var var ->
    let* var = get_var var.bind in
    (match var with
    | Fresh fresh ->
      (* TODO: Check this case. *)
      validate_app_param fresh.lower
    | Rigid rigid ->
      (* Both bounds have the same kind in a well-formed type variable. *)
      validate_app_param rigid.lower)
  | Abs abs ->
    return (abs.param.lower, abs.param.upper)
  | Union union ->
    (* Both operands have the same kind in a well-formed union. *)
    validate_app_param union.left
  | Inter inter ->
    (* Both operands have the same kind in a well-formed intersection. *)
    validate_app_param inter.left
  | _ ->
    invalid_arg "validate_app_param"

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
    return (Bot, Top)
  in
  let* cond = System.isa lower upper in
  if not cond then
    Error.validate_interval param.interval lower upper
  else
  return { bind = param.bind; lower; upper }

and validate_param_with param f =
  let* param = validate_param param in
  let* type' = with_param_rigid param f in
  return (param, type')
