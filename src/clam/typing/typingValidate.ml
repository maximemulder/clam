open Model

let rec validate_proper type' =
  validate type';
  if TypingKind.get_kind type' <> TypingKind.Type then
    TypingErrors.raise_validate_proper type'
  else
  ()

and validate_subtype type' constr =
  validate type';
  if not (Typing.isa type' constr) then
    TypingErrors.raise_validate_subtype type' constr
  else
  ()

and validate_suptype type' constr =
  validate type';
  if not (Typing.isa constr type') then
    TypingErrors.raise_validate_suptype type' constr
  else
  ()

and validate (type': type') =
  match type' with
  | TypeTop _    -> ()
  | TypeBot _    -> ()
  | TypeUnit _   -> ()
  | TypeBool _   -> ()
  | TypeInt _    -> ()
  | TypeChar _   -> ()
  | TypeString _ -> ()
  | TypeVar _    -> ()
  | TypeAbsExpr abs ->
    validate_proper abs.param;
    validate_proper abs.body;
  | TypeAbsExprType abs ->
    validate_param abs.param;
    validate_proper abs.body;
  | TypeTuple tuple ->
    validate_tuple tuple
  | TypeRecord record ->
    validate_record record;
  | TypeInter inter ->
    validate_inter inter;
  | TypeUnion union ->
    validate_union union;
  | TypeAbs abs ->
    validate_param abs.param;
    validate abs.body;
  | TypeApp app ->
    validate_app app;

and validate_tuple tuple =
  List.iter validate_proper tuple.elems

and validate_record record =
  Utils.NameMap.iter (fun _ attr -> validate_record_attr attr) record.attrs

and validate_record_attr attr =
  validate_proper attr.type'

and validate_inter inter =
  validate_proper inter.left;
  validate_proper inter.right;

and validate_union union =
  validate_proper union.left;
  validate_proper union.right;

and validate_app app =
  match validate_app_type app.type' with
  | Some abs ->
    valiate_app_abs app abs
  | None ->
    TypingErrors.raise_type_app_kind app.type'

and validate_app_type type' =
  match type' with
  | TypeVar var ->
    validate_app_type var.param.bound
  | TypeApp app ->
    validate_app_type (TypingApp.apply_app app)
  | TypeAbs abs ->
    Some abs
  | _ ->
    None

and valiate_app_abs app abs =
  validate_subtype app.arg abs.param.bound;
  validate abs.body

and validate_param param =
  validate param.bound
