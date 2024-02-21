open Node

let rec validate_proper ctx type' =
  let type'' = validate ctx type' in
  if Kind.get_kind ctx type'' <> Kind.Type then
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
  if Kind.get_kind ctx left <> Kind.get_kind ctx right then
    Error.validate_union_kind union
  else
  System.join ctx left right

and validate_inter ctx inter =
  let left  = validate ctx inter.left  in
  let right = validate ctx inter.right in
  if Kind.get_kind ctx left <> Kind.get_kind ctx right then
    Error.validate_inter_kind inter
  else
  System.meet ctx left right

and validate_app_param ctx abs =
  validate_app_param_union ctx abs

and validate_app_param_union ctx union =
  let types = List.map (validate_app_param_inter ctx) union.union in
  Util.list_reduce (System.meet ctx) types

and validate_app_param_inter ctx inter =
  let types = List.map (validate_app_param_base ctx) inter.inter in
  Util.list_reduce (System.join ctx) types

and validate_app_param_base ctx type' =
  match type' with
  | Var var ->
    let bound = Context.get_bind_type ctx var.bind in
    validate_app_param ctx bound
  | Abs abs ->
    abs.param.bound
  | _ ->
    invalid_arg "validate_app_param_base"

and validate_param_with ctx param f =
  let bound = validate ctx param.bound in
  let param = { bind = param.bind; bound } in
  let other = f (Context.add_bind_type ctx param.bind param.bound) in
  param, other
