open Node

let rec validate_proper ctx type' =
  let type'' = validate ctx type' in
  if Kind.get_kind ctx type'' <> Type then
    Error.validate_proper type'
  else
    type''

and validate ctx (type': Abt.type') =
  match type' with
  | TypeTop    _ -> base Top
  | TypeBot    _ -> base Bot
  | TypeUnit   _ -> base Unit
  | TypeBool   _ -> base Bool
  | TypeInt    _ -> base Int
  | TypeString _ -> base String
  | TypeVar var ->
    base (Var { bind = var.bind })
  | TypeTuple tuple ->
    validate_tuple ctx tuple
  | TypeRecord record ->
    validate_record ctx record
  | TypeLam lam ->
    validate_lam ctx lam
  | TypeUniv univ ->
    validate_univ ctx univ
  | TypeAbs abs ->
    validate_abs ctx abs
  | TypeApp app ->
    validate_app ctx app
  | TypeUnion union ->
    validate_union ctx union
  | TypeInter inter ->
    validate_inter ctx inter

and validate_tuple ctx tuple =
  let elems = List.map (validate_proper ctx) tuple.elems in
  base (Tuple { elems })

and validate_record ctx record =
  let attrs = Util.NameMap.map (validate_record_attr ctx) record.attrs in
  base (Record { attrs })

and validate_record_attr ctx attr =
  let type' = validate_proper ctx attr.type' in
  { label = attr.label; type' }

and validate_lam ctx lam =
  let param = validate_proper ctx lam.param in
  let ret = validate_proper ctx lam.ret in
  base (Lam { param; ret })

and validate_univ ctx univ =
  let param, ret = validate_param_with ctx univ.param
    (fun ctx -> validate_proper ctx univ.ret) in
  base (Univ { param; ret })

and validate_abs ctx abs =
  let param, body = validate_param_with ctx abs.param
    (fun ctx -> validate ctx abs.body) in
  base (Abs { param; body })

(* TODO: Handle lower bounds *)
and validate_app ctx app =
  let abs = validate ctx app.abs in
  let arg = validate ctx app.arg in
  let param = validate_app_param ctx abs in
  if not (System.isa ctx arg param) then
    Error.validate_app_arg app param arg
  else
  System.compute ctx abs arg

and validate_union ctx union =
  let left  = validate ctx union.left  in
  let right = validate ctx union.right in
  if not (System.is_kind ctx (Kind.get_kind ctx left) (Kind.get_kind ctx right)) then
    Error.validate_union_kind union
  else
  System.join ctx left right

and validate_inter ctx inter =
  let left  = validate ctx inter.left  in
  let right = validate ctx inter.right in
  if not (System.is_kind ctx (Kind.get_kind ctx left) (Kind.get_kind ctx right)) then
    Error.validate_inter_kind inter
  else
  System.meet ctx left right

and validate_app_param ctx abs =
  match abs with
  | Dnf dnf ->
    validate_app_param_union ctx dnf
  | Cnf _ ->
    raise (invalid_arg "TODO")

and validate_app_param_union ctx union =
  let types = List.map (validate_app_param_inter ctx) union in
  Util.list_reduce (System.meet ctx) types

and validate_app_param_inter ctx inter =
  let types = List.map (validate_app_param_base ctx) inter in
  Util.list_reduce (System.join ctx) types

and validate_app_param_base ctx type' =
  match type' with
  | Var var ->
    let _, upper = Context.get_bounds ctx var.bind in
    validate_app_param ctx upper
  | Abs abs ->
    abs.param.upper
  | _ ->
    invalid_arg "validate_app_param_base"

and validate_param ctx (param: Abt.param_type) =
  let lower, upper = match param.interval.lower, param.interval.upper with
  | Some lower, Some upper ->
    validate ctx lower, validate ctx upper
  | Some lower, None ->
    let lower = validate ctx lower in
    let kind = Kind.get_kind ctx lower in
    let upper = Kind.get_kind_max ctx kind in
    lower, upper
  | None, Some upper ->
    let upper = validate ctx upper in
    let kind = Kind.get_kind ctx upper in
    let lower = Kind.get_kind_min ctx kind in
    lower, upper
  | None, None ->
    Node.bot, Node.top
  in
  if not(System.isa ctx lower upper) then
    Error.validate_interval param.interval lower upper
  else
  { bind = param.bind; lower; upper }

and validate_param_with ctx param f =
  let param = validate_param ctx param in
  let ctx = Context.add_param ctx param in
  let other = f ctx in
  param, other
