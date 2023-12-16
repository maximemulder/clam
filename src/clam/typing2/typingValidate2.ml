open Type
open TypingErrors2

let rec validate_proper ctx type' =
  let type'' = validate ctx type' in
  if TypingKind2.get_kind ctx type'' <> TypingKind2.Type then
    TypingErrors2.raise_validate_proper type'
  else
    type''

and validate ctx (type': Model.type') =
  match type' with
  | TypeTop { pos } ->
    base (Top { pos })
  | TypeBot { pos } ->
    base (Bot { pos })
  | TypeUnit { pos } ->
    base (Unit { pos })
  | TypeBool { pos } ->
    base (Bool { pos })
  | TypeInt { pos } ->
    base (Int { pos })
  | TypeChar { pos } ->
    base (Char { pos })
  | TypeString { pos } ->
    base (String { pos })
  | TypeVar var ->
    base (Var { pos = var.pos; bind = var.bind })
  | TypeTuple tuple ->
    validate_tuple ctx tuple
  | TypeRecord record ->
    validate_record ctx record
  | TypeInter inter ->
    validate_inter ctx inter
  | TypeUnion union ->
    validate_union ctx union
  | TypeAbsExpr abs ->
    validate_abs_expr ctx abs
  | TypeAbsExprType abs ->
    validate_abs_type_expr ctx abs
  | TypeAbs abs ->
    validate_abs ctx abs
  | TypeApp app ->
    validate_app ctx app

and validate_tuple ctx tuple =
  let elems = List.map (validate_proper ctx) tuple.elems in
  base (Tuple { pos = tuple.pos; elems })

and validate_record ctx record =
  let attrs = Utils.NameMap.map (validate_record_attr ctx) record.attrs in
  base (Record { pos = record.pos; attrs })

and validate_record_attr ctx attr =
  let type' = validate_proper ctx attr.type' in
  { pos = attr.pos; name = attr.name; type' }

and validate_inter ctx inter =
  let left  = validate ctx inter.left  in
  let right = validate ctx inter.right in
  if TypingKind2.get_kind ctx left <> TypingKind2.get_kind ctx right then
    raise_validate_inter_kind inter
  else
  Typing2.meet ctx left right

and validate_union ctx union =
  let left  = validate ctx union.left  in
  let right = validate ctx union.right in
  if TypingKind2.get_kind ctx left <> TypingKind2.get_kind ctx right then
    raise_validate_union_kind union
  else
  Typing2.join ctx left right

and validate_abs_expr ctx abs =
  let param = validate_proper ctx abs.param in
  let ret = validate_proper ctx abs.body in
  base (AbsExpr { pos = abs.pos; param; ret })

and validate_abs_type_expr ctx abs =
  let param = validate_param ctx abs.param in
  let ret = validate_proper ctx abs.body in
  base (AbsTypeExpr { pos = abs.pos; param; ret })

and validate_abs ctx abs =
  let param = validate_param ctx abs.param in
  let body = validate ctx abs.body in
  base (Abs { pos = abs.pos; param; body })

and validate_app ctx app =
  let abs = validate ctx app.type' in
  let arg = validate ctx app.arg in
  (* TODO:
    1. Get param from abs (throw error if no param)
    2. check bounds (if needed, not sure)
    3. arg isa param (throw error if no)
    4. compute type
  *)
  validate_app_type ctx abs arg

and validate_app_type ctx type' arg =
  validate_app_union ctx type' arg

and validate_app_union ctx union arg =
  let types = List.map ((Utils.flip (validate_app_inter ctx)) arg) union.union in
  Utils.reduce_list (Typing2.join ctx) types

and validate_app_inter ctx inter arg =
  let types = List.map ((Utils.flip (validate_app_base ctx) arg)) inter.inter in
  Utils.reduce_list (Typing2.meet ctx) types

and validate_app_base ctx type' arg =
  match type' with
  | Var var ->
    base (App { pos = pos type'; abs = base (Var var); arg })
  | Abs abs ->
    let entry = TypingContext2.entry abs.param.bind arg in
    Typing2.substitute ctx entry abs.body
  | _ ->
    invalid_arg "validate_app_base"

and validate_param ctx param =
  let bound = validate ctx param.bound in
  { bind = param.bind; bound }
